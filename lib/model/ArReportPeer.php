<?php

require 'lib/model/om/BaseArReportPeer.php';


class ArReportPeer extends BaseArReportPeer
{

    ///////////////////////////////////////////////
    // ACCESS REPORTS ACCORDING USER PERMISSIONS //
    ///////////////////////////////////////////////

    /**
     * Publish or unpublish the report to subscribed users.
     *
     * @param int $reportId
     * @param bool $isPublish true for publishing, false for unpublishing,
     * @param bool $forceResend true if the report is a new version, and must be resent to users,
     * false for sending the report only to new users.
     * @param PDO|null $conn
     * @return void
     * @throws ArProblemException
     * @throws Exception
     *
     */
    static
    public function publishReportToUsers($reportId, $isPublish, $forceResend, $conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        // Check if the report must be regenerated
        if ($isPublish) {
            $stm = $conn->prepare('
               SELECT produced_report_must_be_regenerated
               FROM   ar_report
               WHERE  id = ?');

            $mustBeRegenerated = false;
            $stm->execute(array($reportId));
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                $mustBeRegenerated = ($rs[0] == 1);
            }
            $stm->closeCursor();

            if ($mustBeRegenerated) {
                throw ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::REPORTS,
                    ArProblemResponsible::ADMIN,
                    'this report must be regenerated ' . $reportId,
                    "The report with id $reportId must be generated again, because there are updates in the associated calls.",
                    "This report is not confirmed as ready to be sent to end users, and it remains in draft status.",
                    "Regenerate the report or the associated report-set.");
            }
        }

        // Change state of the report

        $conn->beginTransaction();
        try {
            $state = $conn->prepare('UPDATE ar_report SET produced_report_already_reviewed = ? WHERE id = ?');
            $state->execute(array($isPublish, $reportId));

            if ($isPublish) {

                // First remove queued reports that are not visible anymore,
                // because maybe report params or user settings are changed.

                $delByPerm = $conn->prepare('
            DELETE ar_report_to_read
            FROM ar_report_to_read LEFT JOIN ar_user_can_view_report
            ON (ar_report_to_read.ar_user_id = ar_user_can_view_report .ar_user_id
                AND ar_report_to_read.ar_report_id = ar_user_can_view_report.ar_report_id)
            WHERE ar_report_to_read.ar_report_id = ?
            AND ar_user_can_view_report.ar_report_id IS NULL
            ');
                $delByPerm->execute(array($reportId));

                // Delete reports that for sure are any more readable from party.
                // NOTE: a legal report must be sent to the legal email of a party.

                $delByLegal = $conn->prepare('
            DELETE ar_report_to_read
            FROM ar_report_to_read
            INNER JOIN ar_report
            ON ar_report_to_read.ar_report_id = ar_report.id
            WHERE ar_report_to_read.ar_report_id = ?
            AND ar_report.param_is_legal = 0
            AND ar_report_to_read.ar_user_id IS NULL
            ');
                $delByLegal->execute(array($reportId));

                //
                // Add new reports in the queue of the reports to read,
                // that are visible to users.
                //

               $delById = $conn->prepare('DELETE FROM ar_report_to_read WHERE id = ?');

                $ins = $conn->prepare('
            INSERT INTO ar_report_to_read
            SET ar_report_id = ?,
                ar_user_id = ?,
                must_be_sent_to_email = ?
            ');

                $insWithNullUser = $conn->prepare('
            INSERT INTO ar_report_to_read
            SET ar_report_id = ?,
                ar_user_id = NULL,
                must_be_sent_to_email = ?
            ');

                $stm = $conn->prepare('
            SELECT v.ar_user_id,
                   p.ar_permission_id,
                   q.id,
                   q.seen_or_received_from_user,
                   q.ar_user_id
            FROM ar_user_can_view_report AS v
            LEFT JOIN ar_view_all_user_permissions AS p
            ON v.ar_user_id = p.ar_user_id
            LEFT JOIN ar_report_to_read AS q
            ON (v.ar_user_id = q.ar_user_id AND v.ar_report_id = q.ar_report_id)
            WHERE v.ar_report_id = ?
            AND p.ar_permission_id = ?
            ');

                $stm->execute(array($reportId, ArPermission::CAN_RECEIVE_EMAILS));
                while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                    $userId = $rs[0];
                    $permissionId = $rs[1];
                    $queueId = $rs[2];
                    $oldUserId = $rs[4];
                    $sendMail = !is_null($permissionId);

                    // Before sending delete old info if:
                    // * the resend is forced
                    // * there is a previous send info
                    // * it was sent to a specific user, and not to the billable/responsible party
                    if ($forceResend && (!is_null($queueId)) && (!is_null($oldUserId))) {
                        $delById->execute(array($queueId));
                    }

                    // send again if:
                    // * there were no previous sending of email
                    // * there were previous sending of emails, but a resend is forced
                    if ($forceResend || is_null($queueId)) {
                        $ins->execute(array($reportId, $userId, $sendMail));
                    }
                }
                $stm->closeCursor();

                //
                // Add reports visible to the legal email of a party,
                // and not from users.
                //

               $stm = $conn->prepare('
            SELECT q.id
            FROM ar_report AS r
            LEFT JOIN ar_report_to_read AS q
            ON r.id = q.ar_report_id
            WHERE r.id = ?
            AND   r.param_is_legal = 1
            AND   q.ar_user_id IS NULL
            ');

                $atLeastOneValue = false;
                $stm->execute(array($reportId));
                while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                    $queueId = $rs[0];
                    $atLeastOneValue = true;

                    if ($forceResend && (!is_null($queueId))) {
                        $delById->execute(array($queueId));
                    }

                    if ($forceResend || is_null($queueId)) {
                        $insWithNullUser->execute(array($reportId, true));
                    }
                }
                $stm->closeCursor();

                if (!$atLeastOneValue) {
                    $stm = $conn->prepare('SELECT id FROM ar_report WHERE id = ? AND param_is_legal = 1');
                    $stm->execute(array($reportId));
                    while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                        $insWithNullUser->execute(array($reportId, true));
                    }
                    $stm->closeCursor();
                }

            } else {
                // The report can not be viewed (anymore)

                $delByReportId = $conn->prepare('DELETE FROM ar_report_to_read WHERE ar_report_id = ?');
                $delByReportId->execute(array($reportId));
            }

            $conn->commit();
        } catch (Exception $e) {
            $conn->rollBack();
            throw $e;
        }
    }
} // ArReportPeer

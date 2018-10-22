<?php

require 'lib/model/om/BaseArReportSetPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_report_set' table.
 *
 *
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportSetPeer extends BaseArReportSetPeer
{


    /**
     * @param int $reportSchedulerId
     * @param int $fromDate
     * @param PDO|null $conn
     * @return int
     */
    static
    public function getReportSetIdAssociatedToReportScheduler($reportSchedulerId, $fromDate, $conn)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $stm = $conn->prepare('SELECT id FROM ar_report_set WHERE ar_report_scheduler_id = ? AND from_date = ?');
        $stm->execute(array($reportSchedulerId, fromUnixTimestampToMySQLTimestamp($fromDate)));

        $r = null;
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }

        $stm->closeCursor();

        return $r;
    }

    /**
     * @param int $reportSetId
     * @param PDO $conn
     */
    static
    public function deleteAssociatedReports($reportSetId, PDO $conn)
    {

        $stmt = $conn->prepare('DELETE FROM ar_report WHERE (ar_report_set_id = ? OR about_ar_report_set_id = ?)');
        $stmt->execute(array($reportSetId, $reportSetId));

        $stmt = $conn->prepare('DELETE FROM ar_postponed_report WHERE ar_report_set_id = ?');
        $stmt->execute(array($reportSetId));
    }

    /**
     * @param int $reportSetId
     * @param PDO $conn
     */
    static
    public function confirmAssociatedReports($reportSetId, PDO $conn)
    {
        self::publishReportToUsers($reportSetId, true, false, $conn);
    }

    /**
     * @param int $reportSetId
     * @param bool $isPublish
     * @param bool $forceResend
     * @param PropelPDO $conn
     * @return void
     * @throws ArProblemException
     * @throws Exception
     */
    static
    public function publishReportToUsers($reportSetId, $isPublish, $forceResend, PropelPDO $conn)
    {

        // Check if there are reports that must be regenerated,
        // in case also for only one report, require a complete regeneration of the report set.
        if ($isPublish) {
            $stm = $conn->prepare('
               SELECT COUNT(*)
               FROM   ar_report
               WHERE  (ar_report_set_id = ?
               OR     about_ar_report_set_id = ?)
               AND    produced_report_must_be_regenerated = 1');

            $mustBeRegenerated = false;
            $stm->execute(array($reportSetId, $reportSetId));
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                $mustBeRegenerated = ($rs[0] > 0);
            }
            $stm->closeCursor();

            if ($mustBeRegenerated) {
                throw ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::REPORTS,
                    ArProblemResponsible::ADMIN,
                    'this report-set must be regenerated ' . $reportSetId,
                    "The report-set with id $reportSetId must be generated again, because there are updates in the associated calls.",
                    "The reports of the report-set are not confirmed as ready to be sent to end users, and they will remain in draft status.",
                    "Regenerate the report-set.");
            }
        }

        // Change status
        $conn->beginTransaction();
        try {
            $stmt = $conn->prepare('
            UPDATE ar_report
            SET produced_report_already_reviewed = ?
            WHERE (ar_report_set_id = ?
            OR about_ar_report_set_id = ?)');
            $stmt->execute(array((int)$isPublish, $reportSetId, $reportSetId));

            $stmt = $conn->prepare('UPDATE ar_report_set SET must_be_reviewed = ? WHERE id = ?');
            $stmt->execute(array((int)!$isPublish, $reportSetId));

            $stm = $conn->prepare('
            SELECT ar_report.id
            FROM ar_report
            WHERE (ar_report_set_id = ?
            OR about_ar_report_set_id = ?)');
            $stm->execute(array($reportSetId, $reportSetId));

            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                $reportId = $rs[0];

                ArReportPeer::publishReportToUsers($reportId, $isPublish, $forceResend, $conn);
            }
            $stm->closeCursor();
            $conn->commit();
        } catch (Exception $e) {
            $conn->rollBack();
            throw $e;
        }
    }

} // ArReportSetPeer

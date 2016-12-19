<?php

require 'lib/model/om/BaseArReportToReadPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_report_to_read' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportToReadPeer extends BaseArReportToReadPeer {


    /**
     * @param int $userId
     * @param int $reportId
     * @return bool true if the user can read the specified report
     */
    static
    public function canUserAccessReport($userId, $reportId) {
        $conn = Propel::getConnection();

        $stmt = $conn->prepare('
        SELECT COUNT(*)
        FROM   ar_report_to_read
        WHERE  ar_report_id = ?
        AND    ar_user_id = ?
        ');

        $stmt->execute(array($reportId, $userId));
        $result = $stmt->fetchColumn();
        $r = intval($result);

        return ($r > 0);
    }


} // ArReportToReadPeer

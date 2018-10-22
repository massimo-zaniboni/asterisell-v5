<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class UpdateOfficialCallDateAccordingLastConfirmedReports extends FixedJobProcessor
{

    public function process() {

        $prof = new JobProfiler('call date updates');

        $conn = Propel::getConnection();

        $d1 = $this->getOfficialCallDate($conn);

        // NOTE: consider LEGAL, because there can be maintanance reports (as error report) produced regularly,
        // and that must not update the call date.
        $query = '
        SELECT to_date, id
        FROM   ar_report
        WHERE  produced_report_already_reviewed = 1
        AND    param_is_legal = 1
        AND    to_date IS NOT NULL
        ORDER BY to_date desc LIMIT 1';

        $stm = $conn->prepare($query);
        $stm->execute();
        $r = null;

        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $prof->incrementProcessedUnits();

            $d2 = fromMySQLTimestampToUnixTimestamp($rs[0]);

            if (is_null($d1) || $d1 < $d2) {
                $r = $d2;
                $d1 = $d2;
            }
        }
        $stm->closeCursor();

        if (!is_null($r)) {
            $this->updateOfficialCallDate($r);
        }

        return $prof->stop();
    }

}

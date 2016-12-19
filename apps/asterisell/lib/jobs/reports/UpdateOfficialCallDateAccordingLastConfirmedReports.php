<?php

/* $LICENSE 2009, 2010, 2012:
 *
 * Copyright (C) 2009, 2010, 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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

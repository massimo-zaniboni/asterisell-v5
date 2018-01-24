<?php

/* $LICENSE 2011, 2015:
 *
 * Copyright (C) 2011, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

/**
 * Count CDRs associated to every error.
 */
class GroupCDRSWithErrors extends FixedJobProcessor
{

    public function process()
    {
        $conn = Propel::getConnection();
        $fromDate = self::getOfficialCallDate($conn);
        if (is_null($fromDate)) {
            $fromDate = self::getGlobalStartingDateForCDRProcessinng($conn);
        }

        $prof = new JobProfiler('days in the past');
        $daysInThePast = ceil(abs(time() - $fromDate) / (60 * 60 * 24));
        $prof->addToProcessedUnits($daysInThePast);

        $query = 'UPDATE ar_new_problem P
                  INNER JOIN (SELECT ar_cdr.ar_problem_duplication_key AS k
                              ,      COUNT(ar_cdr.id) AS s
                              FROM ar_cdr
                              WHERE ar_cdr.calldate >= ?
                              AND   ar_cdr.destination_type = ?
                              GROUP by ar_cdr.ar_problem_duplication_key) AS C
                  ON P.duplication_key = C.k
                  SET P.count_of_cdrs = C.s
                  ';
        $stmt = $conn->prepare($query);
        $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($fromDate)
                            , DestinationType::known_error
                        ));
        $stmt->closeCursor();

        return "Counted CDRs with errors of " . $prof->stop();
    }
}

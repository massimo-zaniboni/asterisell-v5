<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

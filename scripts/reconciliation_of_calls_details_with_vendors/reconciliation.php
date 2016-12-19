<?php
/* $LICENSE 2013, 2015:
 *
 * Copyright (C) 2013, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

/*
This application compare and show the differences between
two CSV files containing call-details.

The application during matching of calls, assumes that:
* there can be small differences on calldate
* there can be small differences on duration
* the prefixes of the call destination must match together at least of some digit
* the call source is never matched

Read the code for additional information on the used formats.

*/

$createTables = <<<'SQL'
CREATE TABLE call(
  is_processed INTEGER NOT NULL,
  is_vendor INTEGER NOT NULL,
  matched_with INTEGER,
  source TEXT NOT NULL,
  destination TEXT NOT NULL,
  call_note TEXT NOT NULL,
  calldate INTEGER NOT NULL,
  duration INTEGER NOT NULL,
  cost FLOAT NOT NULL,
  display_order TEXT
);
SQL;

$createIndexes = <<<'SQL'

CREATE INDEX index_1 ON call(is_processed, calldate);

CREATE INDEX index_2 ON call(display_order);

SQL;

$insertStmt = <<<'SQL'
INSERT INTO call VALUES(
           0,
           :is_vendor,
           null,
           :source,
           :destination,
           :call_note,
           :calldate,
           :duration,
           :cost,
           null);
SQL;

// TODO estendere con supporto per la direction della call

main($argc, $argv);
exit(0);

function help()
{
    echo <<<'STRING'

Usage:

  php reconciliation.php format-name vendor-file format-name asterisell-file

available format are:

  ptprime

    "Nº Conta","Factura Nº","Origem","Dia","Mês","Destino","Início","Duração","Imp","Tipo","Segundos","Valor em Euros"
    "1018281610","P002072157","234373200","02","Maio","239405811","07:52:19","00:00:05",0,"N",5,"0,001"
    "1018281610","P002072157","234373200","02","Maio","239405811","08:13:30","00:00:05",0,"N",5,"0,001"

  asterisell

    ,"Account","Direction","Telephone Number","Location","Connection type","Date","Duration in seconds","Currency","Cost (VAT excluded)","Cost Savings (VAT excluded)","Vendor","Communication Channel","Organization Level 1","Organization Level 2","Organization Level 3","Organization Level 4","Organization Level 5","Organization Level 6","Organization Level 7","Organization Level 8","Organization Level 9","Organization Level 10","Organization Level 11","Organization Level 12","Organization Level 13","Organization Level 14","Organization Level 15"
    ,"Universidade de Aveiro / Serviços/ Departamentos internos à UA / Departamento de Química / Eduarda Pereira (24904)","outgoing","234423XXX","Portugal","Fixed Line","2013-05-31 23:19:17","8","EUR","0.0010","0.0000","PT Prime","SIP Fixed Line Operator","Universidade de Aveiro","Serviços/ Departamentos internos à UA","Departamento de Química","Eduarda Pereira (24904)","","","","","","","","","","",""
    ,"Universidade de Aveiro / Instituíções afiliadas à UA / Instituto Superior de Contabilidade e Administração da Universidade de Aveiro (ISCA-UA) / sec-ISCA (45118)","outgoing","234423XXX","Portugal","Fixed Line","2013-05-31 21:57:11","20","EUR","0.0020","0.0000","PT Prime","SIP Fixed Line Operator","Universidade de Aveiro","Instituíções afiliadas à UA","Instituto Superior de Contabilidade e Administração da Universidade de Aveiro (ISCA-UA)","sec-ISCA (45118)","","","","","","","","","","",""
    ,"Universidade de Aveiro / Serviços/ Departamentos internos à UA / Serviços de Ação Social da Universidade de Aveiro (SASUA) / Secretaria de Apoio Ao Estudante (22508)","outgoing","707500XXX","Portugal","Fixed Line","2013-05-31 20:54:00","22","EUR","0.0030","0.0000","PT Prime","SIP Fixed Line Operator","Universidade de Aveiro","Serviços/ Departamentos internos à UA","Serviços de Ação Social da Universidade de Aveiro (SASUA)","Secretaria de Apoio Ao Estudante (22508)","","","","","","","","","","",""

STRING;

}

function main($argc, $argv)
{
    global $createTables;
    global $createIndexes;

    if ($argc !== 5) {
        help();
        return;
    }

    $databaseFile = 'reconcile.sqlite.db';

    @unlink($databaseFile);

    $conn = new SQLite3($databaseFile, SQLITE3_OPEN_READWRITE | SQLITE3_OPEN_CREATE);

    $conn->exec($createTables);

    $conn->exec('BEGIN TRANSACTION;');

    $i = 1;
    $formatType = $argv[$i++];
    $fileName = $argv[$i++];
    importAccordingFormat($formatType, $fileName, true, $conn);

    $formatType = $argv[$i++];
    $fileName = $argv[$i++];
    importAccordingFormat($formatType, $fileName, false, $conn);

    $conn->exec('COMMIT TRANSACTION;');

    $conn->exec($createIndexes);

    $conn->exec('BEGIN TRANSACTION;');

    matchCalls($conn, null);

    $conn->exec('COMMIT TRANSACTION;');

    writeFileWithMatches('reconciliation.csv', true, true, $conn);
}

/**
 * @param SQLite3 $conn
 * @param int|null $limit
 */
function matchCalls(SQLite3 $conn, $limit)
{
    $asterisellIdToInspect = 20100;

    $nextCallToProcessQuery = <<<'SQL'
SELECT ROWID, destination, calldate, duration
FROM  call
WHERE is_vendor = 0
ORDER BY calldate
SQL;

    $nextCallToProcess = $conn->prepare($nextCallToProcessQuery);

    $candidateMatchesQuery = <<<'SQL'
SELECT ROWID, destination, calldate, duration
FROM call
WHERE is_vendor = 1
AND calldate >= :min_calldate
AND calldate <= :max_calldate
AND is_processed = 0
;
SQL;

    $candidateMatches = $conn->prepare($candidateMatchesQuery);

    $executeMatchQuery = <<<'SQL'
UPDATE call
SET is_processed = 1,
    matched_with = :match_with,
    display_order = :display_order
WHERE ROWID = :id
;
SQL;

    $conn->exec('UPDATE call SET display_order = calldate');

    $count = 0;

    $executeMatch = $conn->prepare($executeMatchQuery);
    $rs = $nextCallToProcess->execute();

    $goodMatch = 0;

    while ($data = $rs->fetchArray(SQLITE3_NUM)) {
        $count++;

        if ((!is_null($limit)) && $count > $limit) {

            echo "\n\nWARING: only first $limit rows are comparated. This is a debug param.\n";

            break;
        }

        if ($count % 1000 == 0) {
            echo "\n    Matched $goodMatch calls, on a total of $count .";
        }

        $asterisellCallDate = $data[2];
        $asterisellDestination = $data[1];
        $asterisellId = $data[0];
        $asterisellDuration = $data[3];

        $secondsOfTollerance = 6;
        $candidateMatches->bindValue(':min_calldate', $asterisellCallDate - $secondsOfTollerance, SQLITE3_INTEGER);
        $candidateMatches->bindValue(':max_calldate', $asterisellCallDate + $secondsOfTollerance, SQLITE3_INTEGER);

        if (intval($asterisellId) == $asterisellIdToInspect) {
            echo "\n   Candidate for Asterisell ";
            var_dump($data);
        }

        $rs2 = $candidateMatches->execute();
        $bestFitId = null;
        $bestFitValue = -10000;
        while ($data2 = $rs2->fetchArray(SQLITE3_NUM)) {

            $vendorId = $data2[0];
            $vendorDestination = $data2[1];
            $vendorCalldate = $data2[2];
            $vendorDuration = $data2[3];

            $nrOfMatchedDigits = 0;
            $maxLen = strlen($asterisellDestination);
            $maxLen2 = strlen($vendorDestination);
            if ($maxLen > $maxLen2) {
                $maxLen = $maxLen2;
            }
            while ($nrOfMatchedDigits < $maxLen && substr($asterisellDestination, 0, $nrOfMatchedDigits + 1) == substr($vendorDestination, 0, $nrOfMatchedDigits + 1)) {
                $nrOfMatchedDigits++;
            }

            $fitId = $vendorId;
            $fitValue = $nrOfMatchedDigits * 50 - (abs($asterisellDuration - $vendorDuration) + abs($asterisellCallDate - $vendorCalldate));

            if (intval($asterisellId) == $asterisellIdToInspect) {

                echo "\n   Candidate for Asterisell ";

                var_dump($data);

                echo "  is vendor ";

                var_dump($data2);

                echo "  with fitValue $fitValue, nrOfMachedDigits: $nrOfMatchedDigits";
            }

            if ($nrOfMatchedDigits > 1 && $fitValue > $bestFitValue) {
                $bestFitId = $fitId;
                $bestFitValue = $fitValue;
            }

        }
        $rs2->finalize();

        $displayOrder = $asterisellCallDate;
        if (!is_null($bestFitId)) {
            $goodMatch++;

            $displayOrder .= '-' . $asterisellId;

            $vendorId = $bestFitId;

            $executeMatch->bindValue(':id', $asterisellId, SQLITE3_INTEGER);
            $executeMatch->bindValue(':match_with', $vendorId, SQLITE3_INTEGER);
            $executeMatch->bindValue(':display_order', $displayOrder, SQLITE3_TEXT);
            $executeMatch->execute();

            $executeMatch->bindValue(':id', $vendorId, SQLITE3_INTEGER);
            $executeMatch->bindValue(':match_with', $asterisellId, SQLITE3_INTEGER);
            $executeMatch->bindValue(':display_order', $displayOrder, SQLITE3_TEXT);
            $executeMatch->execute();
        } else {
            $executeMatch->bindValue(':id', $asterisellId, SQLITE3_INTEGER);
            $executeMatch->bindValue(':match_with', null, SQLITE3_NULL);
            $executeMatch->bindValue(':display_order', $displayOrder, SQLITE3_TEXT);
            $executeMatch->execute();
        }
    }
    $rs->finalize();

}

/**
 * @param string $fileName
 * @param bool $writeOnlyCallsWithErrors
 * @param bool $writeOnlySeriousErrors
 * @param SQLite3 $conn
 */
function writeFileWithMatches($fileName, $writeOnlyCallsWithErrors, $writeOnlySeriousErrors, SQLite3 $conn)
{
    $handle = fopen($fileName, 'w+');

    if ($handle === FALSE) {
        echo "\nError writing to $fileName";
        exit(1);
    }

    $query = <<<'SQL'
SELECT ROWID, is_vendor, matched_with,source, destination, calldate, duration, cost, call_note
FROM call
ORDER BY display_order, is_vendor
;
SQL;

    $stmt = $conn->prepare($query);

    fwrite($handle, '"match level","origin","source","destination","calldate","duration","cost","cost difference","duration difference","call start difference","note","rowid"');

    $rs = $stmt->execute();
    $asterisellCalldate = null;
    $asterisellDuration = null;
    $asterisellCost = null;

    $countExceptions = 0;

    $totCallsWithCostDifference = 0;
    $totCostDifference = 0.0;

    $totUnmatchedAsterisellCount = 0;
    $totUnmatchedAsterisellCost = 0.0;

    $totUnmatchedVendorCount = 0;
    $totUnmatchedVendorCost = 0.0;

    $totMatchedCount = 0;
    $totMatchedCost = 0.0;

    $asterisellLine = '';
    $count = 0;
    while ($data = $rs->fetchArray(SQLITE3_NUM)) {

        $count++;

        $i = 0;
        $rowId = $data[$i++];
        $isVendor = $data[$i++];
        $matchedWith = $data[$i++];
        $source = $data[$i++];
        $destination = $data[$i++];
        $calldate = $data[$i++];
        $duration = $data[$i++];
        $cost = $data[$i++];
        $callNote = $data[$i++];

        $callDateLabel = date('Y-m-d H:i:s', intval($calldate));

        $isPerfect = false;
        $isSeriouslyFault = false;

        if ($isVendor == 1) {
            $vendorLabel = 'vendor';
        } else {
            $vendorLabel = 'asterisell';
            $asterisellLine = '';
        }

        if (strlen(trim($matchedWith)) == 0) {
            $isMatched = false;
            $matchLabel = '"NO MATCH"';
            $asterisellLine = '';
        } else {
            $isMatched = true;
            $matchLabel = '""';
        }

        if ($isVendor && $isMatched && (!is_null($asterisellCalldate))) {

            $callDateDiff = intval($asterisellCalldate) - intval($calldate);
            $durationDiff = intval($asterisellDuration) - intval($duration);
            $costDiff = floatval($asterisellCost) - floatval($cost);

            $totCostDifference += $costDiff;

            if ($costDiff == 0 && $callDateDiff < 5 && $durationDiff < 2) {
                $matchLabel = '"perfect"';
                $isPerfect = true;
            } else if ($costDiff == 0 && $callDateDiff < 10 && $durationDiff < 5) {
                $matchLabel = '"near perfect"';
            } else if (abs($costDiff) < 0.005) {
                $matchLabel = '"acceptable"';
            } else {
                $matchLabel = '"to review"';
                $isSeriouslyFault = true;
            }

        } else {
            $callDateDiff = 0;
            $durationDiff = 0;
            $costDiff = 0;
        }

        if (! $isMatched) {
            if ($isVendor) {
                $totUnmatchedVendorCount++;
                $totUnmatchedVendorCost += floatval($cost);
            } else {
                $totUnmatchedAsterisellCount++;
                $totUnmatchedAsterisellCost += floatval($cost);
            }
        } else {
            if ($isVendor) {
                // count only the vendor part, otherwise it doubles the values
                $totMatchedCost += floatval($cost);
                $totMatchedCount++;
            }
        }

        if (!$isVendor) {
            $asterisellCalldate = $calldate;
            $asterisellDuration = $duration;
            $asterisellCost = $cost;
        } else {
            $asterisellCalldate = null;
            $asterisellDuration = null;
            $asterisellCost = null;
        }

        $line = "\n$matchLabel,$vendorLabel,\"$source\",\"$destination\",\"$callDateLabel\",$duration,$cost,$costDiff,$durationDiff,$callDateDiff,\"$callNote\",\"$rowId\"";

        if (!$isVendor) {
            $asterisellLine = $line;
        }

        if (!$writeOnlyCallsWithErrors) {
            fwrite($handle, $line);
        } else if (!$isMatched) {
            fwrite($handle, $line);
        } else if ($isMatched && $isVendor && $writeOnlySeriousErrors && $isSeriouslyFault) {
            fwrite($handle, $asterisellLine);
            fwrite($handle, $line);
        } else if ($isMatched && $isVendor && (! $writeOnlySeriousErrors) && (! $isPerfect)) {
            fwrite($handle, $asterisellLine);
            fwrite($handle, $line);
        }

    }

    $rs->finalize();

    fwrite($handle, "\n");
    fclose($handle);

    echo "\n\nWritten $count lines to $fileName \n";

    $differenceOfCount = $totUnmatchedAsterisellCount - $totUnmatchedVendorCount;
    $differenceOfCost = $totUnmatchedAsterisellCost - $totUnmatchedVendorCost;

    echo "
    Matched $totMatchedCount calls, with a cost of $totMatchedCost
    Cost difference between Vendor and Asterisell for matched calls, is $totCostDifference (positive number you are paying less)

    Total cost difference for $totUnmatchedAsterisellCount  unmatched Asterisell calls is $totUnmatchedAsterisellCost

    Total cost difference for $totUnmatchedVendorCount unmatched vendor calls is $totUnmatchedVendorCost

    The difference between Asterisell invoice, and Vendor invoice should be of $differenceOfCount calls, and $differenceOfCost cost.

    ";

    if ($writeOnlyCallsWithErrors) {
        echo "\n\nWARING: only rows with problems are inserted. This is a debug param.\n";
    }



}

/**
 * @param string $formatType
 * @param string $fileName
 * @param bool $isVendor
 * @param SQLite3 $conn
 */
function importAccordingFormat($formatType, $fileName, $isVendor, $conn)
{
    if ($formatType == 'ptprime') {
        importFromCSV_usingPTPrimeFormat($fileName, $isVendor, $conn);
    } else if ($formatType == 'asterisell') {
        importFromCSV_usingAsterisellFormat($fileName, $isVendor, $conn);
    } else {
        help();
        exit(1);
    }
}


/**
 * @param string $fileName
 * @param bool $isVendor
 * @param SQLite3 $conn
 */
function importFromCSV_usingPTPrimeFormat($fileName, $isVendor, SQLite3 $conn)
{
    global $insertStmt;

    $thereIsHeader = false;

    $s = $conn->prepare($insertStmt);

    if ($isVendor) {
        $isVendorInt = 1;
    } else {
        $isVendorInt = 0;
    }

    $ln = 0;

    $handle = fopen($fileName, 'r');
    while (($data = fgetcsv($handle, 64000, ",", "\"", "\"")) !== FALSE) {
        $ln++;

        if ($ln == 1 && $thereIsHeader) {
            continue;
            // skip header
        }

        if ($ln % 1000 == 0) {
            echo "\n  ... imported $ln records from $fileName";
        }

        $i = 3;
        $dayOfMonth = $data[$i++];
        $i++;
        $calledNr = $data[$i++];
        $time = $data[$i++];
        $i++;
        $i++;
        $callNote = $data[$i++];
        $duration = $data[$i++];
        $cost = $data[$i++];

        $callDate = strtotime("2013-05-$dayOfMonth $time");
        $callCost = str_replace(',', '.', $cost);

        $s->bindValue(':is_vendor', $isVendorInt, SQLITE3_INTEGER);
        $s->bindValue(':source', '', SQLITE3_TEXT);
        $s->bindValue(':destination', $calledNr, SQLITE3_TEXT);
        $s->bindValue(':calldate', $callDate, SQLITE3_INTEGER);
        $s->bindValue(':duration', intval($duration), SQLITE3_INTEGER);
        $s->bindValue(':cost', floatval($callCost), SQLITE3_FLOAT);
        $s->bindValue(':call_note', $callNote, SQLITE3_TEXT);

        $s->execute();
    }

    echo "\nInserted $ln records.";

}


/**
 * @param string $fileName
 * @param bool $isVendor
 * @param SQLite3 $conn
 */
function importFromCSV_usingAsterisellFormat($fileName, $isVendor, SQLite3 $conn)
{
    global $insertStmt;

    $thereIsHeader = true;

    $s = $conn->prepare($insertStmt);

    if ($isVendor) {
        $isVendorInt = 1;
    } else {
        $isVendorInt = 0;
    }

    $ln = 0;

    $handle = fopen($fileName, 'r');
    while (($data = fgetcsv($handle, 64000, ",", "\"", "\"")) !== FALSE) {
        $ln++;


        if ($ln == 1 && $thereIsHeader) {
            continue;
            // skip header
        }

        if ($ln % 1000 == 0) {
            echo "\n  ... imported $ln records from $fileName";
        }

        $i = 1;
        $source = $data[$i++];
        $i++;
        $calledNr = $data[$i++];
        $geographicLocation = $data[$i++];
        $connectionType = $data[$i++];
        $callDate = strtotime($data[$i++]);
        $duration = $data[$i++];
        $i++;
        $callCost = $data[$i++];

        // replace international numbers with 00 prefix
        if (substr($calledNr, 0, 1) == '+') {
            $calledNr = '00' . substr($calledNr, 1);
        }

        $callNote = $geographicLocation . ' - ' . $connectionType;

        $s->bindValue(':is_vendor', $isVendorInt, SQLITE3_INTEGER);
        $s->bindValue(':source', $source, SQLITE3_TEXT);
        $s->bindValue(':destination', $calledNr, SQLITE3_TEXT);
        $s->bindValue(':calldate', $callDate, SQLITE3_INTEGER);
        $s->bindValue(':duration', intval($duration), SQLITE3_INTEGER);
        $s->bindValue(':cost', floatval($callCost), SQLITE3_FLOAT);
        $s->bindValue(':call_note', $callNote, SQLITE3_TEXT);

        $s->execute();
    }

    echo "\nInserted $ln records.";

}


<?php

/* $LICENSE 2009, 2010, 2014:
 *
 * Copyright (C) 2009, 2010, 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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


/**
 * Manage NumberPortability.
 * This process works in a better way according the used heuristic if ported telephone numbers are signaled following
 * the date of the porting.
 */
class NumberPortability
{
    /**
     * Import data inside the file.
     *
     * @param resource $handle the file to process.
     * @param bool $useCSV True for CSV format, False for TWT format.
     *
     * @pre $handle is a readable and valid file.
     * @post $handle will be closed.
     *
     * @return int|string int the number of new ported telephone numbers,
     * or a String with the error message in case of errors
     * during processing.
     */
    public function executeImportCSV($handle, $useCSV)
    {
        $conn = Propel::getConnection();
        $conn->beginTransaction();

        $count1 = ArNumberPortabilityPeer::doCount(new Criteria(), false, $conn);

        if ($useCSV == TRUE) {
            $processOk = $this->processCSVFile($handle, $conn);
        } else {
            $processOk = $this->processTWTFile($handle, $conn);
        }
        fclose($handle);

        $count2 = ArNumberPortabilityPeer::doCount(new Criteria(), false, $conn);
        $insertedRecords = $count2 - $count1;

        $conn->commit();

        if (!is_null($processOk)) {
            return "Inserted $insertedRecords records. The importing was suspend by this error: " . $processOk;
        } else {
            return $insertedRecords;
        }

    }

    /**
     * @param $handle
     * @param PropelPDO $conn
     * @return null|string error message in case of errors
     */
    public function processCSVFile($handle, PropelPDO $conn)
    {
        $nrOfCol = 3;

        $ln = 0;

        while (($data = fgetcsv($handle, 64000, ",", "\"", "\"")) !== FALSE) {
            $ln++;
            $nr = count($data);
            if ($nr != $nrOfCol) {
                return "at line $ln there are $nr columns instead of expected $nrOfCol";
            } else {
                $sourceNumber = $data[0];
                $destNumber = $data[1];
                $fromDate = strtotime($data[2]);

                $this->maybeInsertNumber($sourceNumber, $destNumber, $fromDate, $conn);
            }
        }

        return null;
    }

    /**
     * @param $handle
     * @param PropelPDO $conn
     *
     * @return null|string
     */
    public function processTWTFile($handle, PropelPDO $conn)
    {
        $nrOfCol = 13;

        $ln = 0;

        // PHP BUG: sto usando apice anche se nei file non c'e`,
        // se non lo uso non mi processa il file!!!
        while (($data = fgetcsv($handle, 50000, ";", "\"")) !== FALSE) {
            $ln++;
            $nr = count($data);
            if ($nr != $nrOfCol) {
                return "at line $ln there are $nr columns instead of expected $nrOfCol";
            } else {

                $callDate = strtotime($data[2]);
                $calledNr = trim($data[4]);
                $truePrefix = trim($data[5]);

                if (substr($calledNr, 0, 2) === "00") {
                    $calledNr = substr($calledNr, 2);
                }

                // replace the initial part of $calledNr with the $truePrefix.
                $destNumber = $truePrefix . substr($calledNr, strlen($truePrefix));

                // NOTE: call in any case, because maybe a previous ported number, is now not any more ported.
                $this->maybeInsertNumber($calledNr, $destNumber, $callDate, $conn);
            }
        } // while

        return null;
    }

    /**
     * Insert the telephone number only if it is a real porting of a new number.
     * In case the number existed before as porting, and now it is not anymore a porting,
     * signal it on the table.
     *
     * @param string $sourceNumber
     * @param string $destNumber
     * @param int $fromDate
     * @param PropelPDO $conn
     */
    public function maybeInsertNumber($sourceNumber, $destNumber, $fromDate, PropelPDO $conn)
    {

        //
        // Prepare queries
        //

        static $stmtScan = null;
        static $stmtInsert = null;
        static $stmtUpdate = null;

        if (is_null($stmtScan)) {
            $stmtScan = $conn->prepare('
                SELECT id, ported_telephone_number, from_date
                FROM ar_number_portability
                WHERE telephone_number = ?
                ORDER BY from_date ASC
                ');

            $stmtInsert = $conn->prepare('
            INSERT INTO ar_number_portability(telephone_number, ported_telephone_number, from_date)
            VALUES (?, ?, ?)
            ');

            $stmtUpdate = $conn->prepare('
            UPDATE ar_number_portability
            SET from_date = ?
            WHERE id = ?
            ');
        }

        //
        // Scan all porting information about the number. Usually they are very few.
        // Annotate only the before, current and after information about the ported telephone number.
        // before, after are about $fromDate
        //

        $beforeId = null;
        $beforePorted = null;
        $beforeIsTheSame = null;

        $afterId = null;
        $afterPorted = null;
        $afterIsTheSame = null;

        $thereIsInfo = false;

        $stmtScan->execute(array($sourceNumber));

        while (($rs = $stmtScan->fetch(PDO::FETCH_NUM)) !== false) {
            $infoId = $rs[0];
            $infoPorted = $rs[1];
            $infoFromDate = fromMySQLTimestampToUnixTimestamp($rs[2]);

            $thereIsInfo = true;

            if ($infoFromDate <= $fromDate) {
                // NOTE: substitute eventually last info, with this info that is nearest to the number

                $beforeId = $infoId;
                $beforePorted = $infoPorted;
                if ($beforePorted == $destNumber) {
                    $beforeIsTheSame = true;
                } else {
                    $beforeIsTheSame = false;
                }
            } else {
                assert($infoFromDate > $fromDate);

                if (is_null($afterId)) {
                    // NOTE: the nearest value is the first, so write it only one time
                    $afterId = $infoId;
                    $afterPorted = $infoPorted;
                    if ($afterPorted == $destNumber) {
                        $afterIsTheSame = true;
                    } else {
                        $afterIsTheSame = false;
                    }
                }
            }
        }
        $stmtScan->closeCursor();

        //
        // Write the information, according the read values, avoiding inserting too much information in the table.
        //


        if (!$thereIsInfo) {
            if ($sourceNumber == $destNumber) {
                // nothing to do, the numbers are the same
            } else {
                // insert for sure, because there is no previous info about the number
                $stmtInsert->execute(array($sourceNumber, $destNumber, fromUnixTimestampToMySQLTimestamp($fromDate)));
                $stmtInsert->closeCursor();
            }
        } else {
            // there is already a porting of the telephone number

            if (!is_null($afterId)) {
                // there is info in the future

                if ($afterIsTheSame) {
                    // this new telephone number is a better aproximation of the ported telephone number.
                    // This is not exact science, it is an heuristic, but it works correctly if porting are signaled
                    // according the from-date.
                    $stmtUpdate->execute(array(fromUnixTimestampToMySQLTimestamp($fromDate), $afterId));
                    $stmtUpdate->closeCursor();
                } else {
                    // in the future there is a different porting, then watch the past...

                    if (!is_null($beforeId)) {
                        if ($beforeIsTheSame) {
                            // nothing to do: there is already this info in the past
                        } else {
                            // insert because this is more recent informatio about the past...
                            $stmtInsert->execute(array($sourceNumber, $destNumber, fromUnixTimestampToMySQLTimestamp($fromDate)));
                            $stmtInsert->closeCursor();
                        }
                    } else {
                        // insert because in the past there wereno info
                        $stmtInsert->execute(array($sourceNumber, $destNumber, fromUnixTimestampToMySQLTimestamp($fromDate)));
                        $stmtInsert->closeCursor();
                    }
                }
            } else {
                // there is no info in the future, then watch the past...

                assert(!is_null($beforeId));

                if ($beforeIsTheSame) {
                    // nothing to do: there is already this info in the past
                } else {
                    // insert because in the past there were not any more current information
                    $stmtInsert->execute(array($sourceNumber, $destNumber, fromUnixTimestampToMySQLTimestamp($fromDate)));
                    $stmtInsert->closeCursor();
                }
            }
        }
    }
}

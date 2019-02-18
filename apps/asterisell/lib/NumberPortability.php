<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>


/**
 * Manage NumberPortability.
 * This process works in a better way according the used heuristic if ported telephone numbers are signaled following
 * the date of the porting, but it can merge different dates, compacting them.
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
     * Insert the telephone number only if it is a real porting of a new number:
     * - merge dates in case it is possible, instead of inserting repeated stupid portings
     * - recognize when the number is not any more ported
     *
     * @param string $sourceNumber
     * @param string $destNumber
     * @param int $fromDate
     * @param PropelPDO $conn
     */
    public function maybeInsertNumber($sourceNumber, $destNumber, $fromDate, PropelPDO $conn)
    {
        $stmt = $conn->prepare("CALL add_ported_telephone_number(?,?,?)");
        $stmt->execute(array($sourceNumber, $destNumber, fromUnixTimestampToMySQLTimestamp($fromDate)));
        $stmt->closeCursor();
    }
}

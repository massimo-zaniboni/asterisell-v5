<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * Set HackedCalls.
 */
class HackedCalls
{
    /**
     * @param resource $handle the file to process.
     *
     * @pre $handle is a readable and valid file.
     * @post $handle will be closed.
     *
     * @return aray(int, int)|string the number of calls on the CSV file, and of recognized calls;
     * or a String with the error message in case of errors.
     */
    public function executeImportCSV($handle)
    {
        $conn = Propel::getConnection();
        $conn->beginTransaction();
        try {
          $nrOfRecords = $this->processCSVFile($handle, $conn);
          fclose($handle);

          if (is_array($nrOfRecords)) {
            $conn->commit();
            return $nrOfRecords;
          } else {
            $conn->rollback();
            return $nrOfRecords;
          }
         } catch (Exception $e) {
          $conn->rollback();
          fclose($handle);
          return $e->getMessage();
        }
   }

   static public function showResult($arr) {
    list($nrOfRecords, $modifiedRecords) = $arr;
    $notRecognized = $nrOfRecords - $modifiedRecords;

    $r = "Set $modifiedRecords hacked calls. $notRecognized not found calls, on a total of $nrOfRecords calls listed in the CSV file.";

    return $r;
   }

    /**
     * @param $handle
     * @param PropelPDO $conn
     * @return array(int, innt)|string error message in case of errors
     */
    public function processCSVFile($handle, PropelPDO $conn)
    {
        $stmt = $conn->prepare("UPDATE ar_source_cdr SET is_hacked = 1 WHERE calldate = ? AND id = ?");
        $stmt2 = $conn->prepare("SELECT id FROM ar_source_cdr WHERE calldate = ? AND id = ?");

        $countModified = 0;

        $nrOfCol = 2;
        $ln = 0;
        while (($data = fgetcsv($handle, 64000, ",", "\"", "\"")) !== FALSE) {
            $ln++;
            $nr = count($data);
            if ($nr != $nrOfCol) {
                return "at line $ln there are $nr columns instead of expected $nrOfCol";
            } else {
                if ($ln > 1) {
                  $calldate = $data[0];
                  $sourceId = $data[1];
                  $q = array($calldate, intval($sourceId));
                  $stmt->execute($q);

                  $stmt2->execute($q);
                  $r = null;
                  while (($rs = $stmt2->fetch(PDO::FETCH_NUM)) !== false) {
                    $countModified++;
                  }
                  $stmt2->closeCursor();

                }
            }
        }

        return array($ln - 1, $countModified);
    }
}

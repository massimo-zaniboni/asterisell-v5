<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Change/import Wholesale information from a CSV file in the format
 * described in the Asterisell manual `doc/manual/topics/web-ui/wholesale_numbers.md`.
 * These CSV files are usually generated from code in `apps/asterisell/modules/wholesale_number_transaction/templates/exportToCsvSuccess.php`
 */
class ChangeWholesaleInfo extends Service
{

    /**
     * A fast function to call (run only if there is data to process)
     * for updating Wholesale info.
     * The procedure put a lock.
     * Read queries should run on good isolation levels for recognizing the locks.
     * NOTE: locks can be only set by a connection, and not from SQL procedures,
     * so it must be a PHP code.
     */
    static public function updateWholesaleInfo()
    {
        $conn = Propel::getConnection();
        self::lockTables($conn);
        try {
            $cmd = $conn->prepare('CALL proc_update_wholesale_transaction()');
            $cmd->execute();
        } catch (Exception $e) {
            self::rollbackAndUnlockTables($conn);
            throw($e);
        }
        self::commitAndUnlockTables($conn);
    }

    // ----------------------------
    // Wholesale lock management

    /**
     * To call before updating the wholesale related tables.
     * The lock overview is (hopefully) this:
     * - the lock block any reader
     * - table are updated (fast operation)
     * - readers can procedee
     * - the rating engine use a weak isolation level
     * - in any case it obey the entire table lock
     * - in any case it starts with a clean database
     * - the rating engine never read directly wholesale tables, but only organization tables
     * - organization tables are updated by safe cron jobs obeying the lock so they work on a consistent state
     */
    static public function lockTables(PropelPDO $conn)
    {
        // TODO the wholesale number feature is not complete, so lock are not managed correctly
        // up to date

        /*
        $cmd = $conn->prepare('SET autocommit = 0');
        $cmd->execute();

        $cmd = $conn->prepare('
          LOCK TABLES ar_wholesale_number WRITE
                    , ar_wholesale_carrier READ
                    , ar_reseller READ
                    , ar_wholesale_number_transaction WRITE
                    , ar_wholesale_number_imported_date_proc WRITE
                    , ar_wholesale_number_transaction_to_update WRITE
                    , ar_wholesale_update_proc WRITE
                    , ar_wholesale_replace_proc WRITE
        ');
        $cmd->execute();

       */
      $cmd = $conn->prepare('START TRANSACTION');
      $cmd->execute();
     }

    static public function commitAndUnlockTables(PropelPDO $conn)
    {
        $cmd = $conn->prepare('COMMIT');
        $cmd->execute();
        // $cmd = $conn->prepare('UNLOCK TABLES');
        // TODO $cmd->execute();
    }

    static public function rollbackAndUnlockTables(PropelPDO $conn)
    {
        $cmd = $conn->prepare('ROLLBACK');
        $cmd->execute();
        // $cmd = $conn->prepare('UNLOCK TABLES');
        // TODO $cmd->execute();
    }

    // ---------------------------
    // Import data from CSV files

    /**
     * @param resource $handle
     * @return int the number of processed numbers
     * @throws Exception
     */
    public function importFromCSVFile($handle)
    {
        $conn = Propel::getConnection();
        self::lockTables($conn);

        $count = 0;
        try {
            $count = $this->processCSVFile($handle, $conn);
        } catch (Exception $e) {
            fclose($handle);
            self::rollbackAndUnlockTables($conn);
            throw($e);
        }
        fclose($handle);
        self::commitAndUnlockTables($conn);
        return ($count);
    }

    /**
     * @param $handle
     * @param PropelPDO $conn
     * @return int processed lines
     * @throws Exception
     */
    public function processCSVFile($handle, PropelPDO $conn)
    {


        $nrOfCol = 10;
        $ln = 0;

        $cmd0 = $conn->prepare('DELETE FROM ar_wholesale_replace_proc');
        $cmd0->execute();

        $cmd0 = $conn->prepare('UPDATE ar_wholesale_number SET csv_to_delete = 1');
        $cmd0->execute();

        /**
         * @var PDOStatement $cmd1
         */
        $cmd1 = $conn->prepare('REPLACE INTO ar_wholesale_replace_proc SET from_date = ?, ar_reseller_id = ?');
        $cmd2 = $conn->prepare('CALL proc_insert_wholesale_number(?,?,?,?,?,?,?,?,?)');
        $last_isAllEffect = null;
        $last_FromDate = null;

        while (($data = fgetcsv($handle, 64000, ",", "\"", "\"")) !== FALSE) {
            $ln++;
            if ($ln > 1) {
                $nr = count($data);
                if ($nr != $nrOfCol) {
                    throw(new Exception("at line $ln there are $nr columns instead of expected $nrOfCol"));
                } else {
                    $updateNumberTable = true;
                    $updateReplaceTable = true;

                    $cEffect = trim(strtoupper($data[0]));
                    $cFromDate = $data[1];
                    $cResellerCode = $data[4];
                    $cCarrierCode = $data[5];
                    $cNumber = $data[6];
                    $cCost = $data[7];
                    $cIncome = $data[8];
                    $cExtensionCodes = $data[9];

                    if ($cEffect == ArWholesaleNumber::ALL_EFFECT) {
                        $isAllEffect = true;
                        $last_isAllEffect = $isAllEffect;
                    } else if ($cEffect == ArWholesaleNumber::RESELLER_EFFECT) {
                        $isAllEffect = false;
                        $last_isAllEffect = $isAllEffect;
                    } else if (isEmptyOrNull($cEffect) && !is_null($last_isAllEffect)) {
                        $isAllEffect = $last_isAllEffect;
                    } else if ($cEffect == ArWholesaleNumber::COMMENT_EFFECT) {
                        continue;
                    } else {
                        throw(new Exception("at line $ln there is an unrecognized effect \"$cEffect\". ALL or RESELLER is expected. "));
                    }

                    $fromDate = null;
                    if (isEmptyOrNull($cFromDate)) {
                        if (is_null($last_FromDate)) {
                            throw(new Exception("at line $ln there is an empty \"from Timestamp\", but there is no previous value to use."));
                        } else {
                            $fromDate = $last_FromDate;
                        }
                    } else {
                        $fromDate = $cFromDate;
                        $last_FromDate = $fromDate;
                        if (is_null($fromDate)) {
                            throw(new Exception("at line $ln there is a not recognized \"from Timestamp\" value \"$cFromDate\"."));
                        }
                    }

                    $carrier = ArWholesaleCarrierPeer::retrieveByInternalNameCached($cCarrierCode);
                    if (is_null($carrier)) {
                        throw(new Exception("at line $ln there is an unrecognized wholesale carrier \"$cCarrierCode\". "));
                    }

                    $resellerId = null;
                    if ($cResellerCode == ArWholesaleNumber::DELETE_SPECIAL_CODE) {
                        $cmd2->execute(array(
                            $cNumber
                        , $fromDate
                        , false // exists
                        , null // extension codes
                        , true // use default extension codes
                        , null // reseller
                        , $carrier->getId()
                        , 0 // cost
                        , 0 // income
                        ));

                        $updateNumberTable = false;
                        $updateReplaceTable = $isAllEffect;
                        // This is an hack because we have no reseller info,
                        // but in normal use it does not pose problems because errors in current
                        // transactions are simply removed as lines.

                    } else if ($cResellerCode == ArWholesaleNumber::FREE_SPECIAL_CODE) {
                        $resellerId = null;
                        $updateReplaceTable = $isAllEffect;
                        // NOTE: in case of RESELLER modifications, make the number FREE
                        // but do not overwrite other assignation of the same seller
                        // at the same date.
                        // This is an hack because we have no reseller info,
                        // but in normal use it does not pose problems because errors in current
                        // transactions are simply removed as lines.
                    } else {
                        $reseller = ArResellerPeer::retrieveByInternalNameCached($cResellerCode);
                        if (is_null($reseller)) {
                            return "at line $ln there is an unrecognized reseller \"$cResellerCode\". ";
                        }
                        $resellerId = $reseller->getId();
                    }

                    if ($updateNumberTable) {
                        $cost = from_php_decimal_to_db_decimal($cCost);
                        $income = from_php_decimal_to_db_decimal($cIncome);
                        $useDefaultExtensionCodes = isEmptyOrNull($cExtensionCodes);

                        $cmd2->execute(array(
                            $cNumber
                        , $fromDate
                        , true // exists
                        , ($useDefaultExtensionCodes ? null : $cExtensionCodes) // extension codes
                        , $useDefaultExtensionCodes // use default extension codes
                        , $resellerId // reseller
                        , $carrier->getId()
                        , $cost
                        , $income
                        ));
                    }

                    if ($updateReplaceTable) {
                        if ($isAllEffect) {
                            $v = array($fromDate, null);
                        } else {
                            $v = array($fromDate, $resellerId);
                        }
                        $cmd1->execute($v);
                    }
                }
            }
        }

        $cmd0 = $conn->prepare('
          DELETE w
          FROM ar_wholesale_number AS w
          INNER JOIN ar_wholesale_replace_proc AS r
          ON w.from_date = r.from_date
          WHERE (w.ar_reseller_id = r.ar_reseller_id OR r.ar_reseller_id IS NULL)
          AND   w.csv_to_delete = 1
          ');
        $cmd0->execute();

        return ($ln - 1);
    }
}

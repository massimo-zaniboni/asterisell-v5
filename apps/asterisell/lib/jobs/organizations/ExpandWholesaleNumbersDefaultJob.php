<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * When ar_wholesale_number.extension_codes is NULL, replace with the number.
 * Execute the action on all records.
 */
class ExpandWholesaleNumbersDefaultJob extends FixedJobProcessor
{

    public function process()
    {
        $conn = Propel::getConnection();
        $conn->beginTransaction();

        try {

            $stmt = $conn->prepare('
        UPDATE ar_wholesale_number
        SET   use_default_extension_codes = 1,
              extension_codes = telephone_number
        WHERE use_default_extension_codes IS NULL
        AND   extension_codes IS NULL
        AND   `exists` = 1
        ');

            $stmt->execute();
            $this->commitTransactionOrSignalProblem($conn);
        } catch (ArProblemException $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);

            $p = ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e
                , get_class($this)
                , "Error in " . get_class($this)
                , "Wholesale numbers will be not processed correctly."
                , "Contact the assistance.");
            throw($p);
        }

        return '';
    }
}

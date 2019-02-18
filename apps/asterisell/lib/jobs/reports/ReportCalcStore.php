<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Contains values to use for producing a report.
 *
 * CONTRACTS:
 * A single store object can be used/shared from different reports of the same report set.,
 * In this way the store can be calculated only one time, and then used from reports of the
 * same type, instantiated from the scheduler, for example:
 * - invoices
 * - billing reports
 * In other words, a ReportSet can reuse the same report store.
 *
 * NOTE if a report requires only the data of an organization,
 * this approach load the data of all organizations,
 * but in practice:
 * - this work is done on the MySQL side and it is rather efficient in any case,
 * because all the CDRs in the data range must be scan in any case,
 * - usually invoices and other reports are generated in batch sessions, and info must be reused
 * between different organizations, and in these cases the calculated info can be reused
 */
class ReportCalcStore
{


    // ---------------------------------------------------------
    // Standard API
    // NOTE: in reality the majority of logic is in the report-generator.

    /**
     * @return bool true if the store contains no data in the specified time frame
     */
    public function isEmpty()
    {
        return false;
    }

    // ------------------------------------------
    // Manage Character Set

    protected $characterSet = null;

    public function setCharacterSet($s)
    {
        $this->characterSet = $s;
    }

    public function getCharacterSet()
    {
        return $this->characterSet;
    }

    /**
     * @param mixed $value a string in UTF-8 format
     * @return string in PDF format
     */
    public function convertString($value)
    {
        if ($this->characterSet == "UTF-8") {
            return $value;
        } else {
            return iconv("UTF-8", $this->characterSet, "$value");
        }
    }
}


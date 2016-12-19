<?php

/* $LICENSE 2012, 2013:
 *
 * Copyright (C) 2012, 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 *
 */
class ReportCalcStore
{

    /**
     * @return bool true if the store contains no data in the specified time frame
     */
    public function isEmpty() {
        return false;
    }

    protected $characterSet = null;

    public function setCharacterSet($s)
    {
        $this->characterSet = $s;
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


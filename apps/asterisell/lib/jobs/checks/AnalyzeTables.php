<?php

/* $LICENSE 2018:
 *
 * Copyright (C) 2018 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Force an explicit analysis of important tables.
 * NOTES: in any case the TokuDB engine is configured for analyzing tables when they reach a 30% increase in dimension.
 */
class AnalyzeTables extends FixedJobProcessor
{

    public function process()
    {
        $checkFile = get_class($this);
        $checkLimit = strtotime("-1 day");
        $mutex = new Mutex($checkFile);
        if ($mutex->maybeTouch($checkLimit)) {
            $this->analyzeTable('ar_cdr');
            $this->analyzeTable('ar_cdr');
            $this->analyzeTable('ar_telephone_prefix');
            $this->analyzeTable('ar_report');
            $this->analyzeTable('ar_report_set');
        } else {
            return "ANALYZE TABLES is done daily";
        }
        return '';
    }

    protected function analyzeTable($tableName) {
        $conn = Propel::getConnection();
        $sql = "ANALYZE TABLE " . $tableName;
        $sth = $conn->prepare($sql);
        $sth->execute();
        $res = $sth->fetchAll();
        $sth->closeCursor();
    }
}

<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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
            $this->analyzeTable('ar_cached_grouped_cdr');
            $this->analyzeTable('ar_vendor');
            $this->analyzeTable('ar_communication_channel_type');
            $this->analyzeTable('ar_organization_unit');
            $this->analyzeTable('ar_organization_unit_type');
            $this->analyzeTable('ar_organization_unit_has_structure');
            $this->analyzeTable('ar_party');
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

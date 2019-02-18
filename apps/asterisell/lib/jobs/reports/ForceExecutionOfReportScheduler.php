<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class ForceExecutionOfReportScheduler extends JobData
{

    /**
     * @var int
     */
    public $arReportSchedulerId;

    /**
     * @var int the date from wich generate the report
     */
    public $fromDate;

    public function getDescription()
    {
        $id = $this->arReportSchedulerId;
        return "Generate reports according the params of ar_report_scheduler.id " . $id . " at date " . fromUnixTimestampToMySQLTimestamp($this->fromDate);
    }
}

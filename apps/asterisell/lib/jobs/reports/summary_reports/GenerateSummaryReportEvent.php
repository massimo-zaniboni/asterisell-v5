<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class GenerateSummaryReportEvent extends JobData
{
    public $reportSetId = null;

    public function getDescription()
    {
        return __("The report set with id " . $this->reportSetId . " was generated.");
    }
}

?>
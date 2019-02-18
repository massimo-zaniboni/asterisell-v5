<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Check and report the CDRs with errors.
 */
class ReportCDRsWithErrors extends FixedJobProcessor
{
    public function process()
    {
        return CustomCDRServices::getInstance()->processInformingOfCDRsWithErrors();
    }
}

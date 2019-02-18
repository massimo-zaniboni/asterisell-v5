<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A billing report where the user can specify all the params.
 */
class BillingReport_UserDefined extends BaseBillingReport
{

    ///////////////////////////////
    // ReportGenerator Interface //
    ///////////////////////////////


    public function deriveReportParams()
    {
        $this->checkParamFromDateExists();
    }
}

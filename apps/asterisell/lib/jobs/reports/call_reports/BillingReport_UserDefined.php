<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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

<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class EmptyReportCalcStore extends ReportCalcStore
{
    public function isEmpty() {
        return false;
    }
}

<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Use GenerateInvoiceUsingTemplate01PDF for generating a legal invoice.
 */
class GenerateLegalInvoiceUsingTemplate01PDF extends GenerateInvoiceUsingTemplate01PDF {

    public function deriveReportParams() {
        $this->setIsLegal(true);
        parent::deriveReportParams();
    }
}

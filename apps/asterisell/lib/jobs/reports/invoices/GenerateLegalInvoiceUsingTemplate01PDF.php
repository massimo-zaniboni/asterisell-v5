<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

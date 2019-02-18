<?php
// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A template for PDF generation of Billing Reports.
 */
class BillingReportPDFTemplate extends ReportPDFTemplate
{

}
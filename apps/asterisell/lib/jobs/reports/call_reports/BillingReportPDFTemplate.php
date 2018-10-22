<?php
// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A template for PDF generation of Billing Reports.
 */
class BillingReportPDFTemplate extends ReportPDFTemplate
{

}
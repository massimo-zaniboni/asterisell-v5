<?php

use_helper('Url', 'I18N');

/**
 * @var ArReportToReadUserView $ArReportToReadUserView
 */

$reportId = $ArReportToReadUserView->getArReportId();

echo link_to(__('download document'), 'viewdocument/download?id=' . $reportId);

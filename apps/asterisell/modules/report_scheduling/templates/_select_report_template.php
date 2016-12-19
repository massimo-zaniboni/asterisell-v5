<?php
/**
 * @var ArReportScheduler $ArReportScheduler
 */

$c = new Criteria();
$c->add(ArReportPeer::IS_TEMPLATE, true);
$c->addAscendingOrderByColumn(ArReportPeer::ID);
$templates = ArReportPeer::doSelect($c);

$options = array('' => '');
foreach ($templates as $template) {
    /**
     * @var ArReport $template
     */
    $options[$template->getId()] = $template->getId() . ' - ' . $template->getName();
}

// Select in the list the current report

$templateId = $ArReportScheduler->getArReportId();

if (is_null($templateId)) {
    $defaultOption = '';
} else {
    $defaultOption = $templateId;
}

// Add selection and related buttons

echo select_tag('select_report_template', options_for_select($options, $defaultOption));


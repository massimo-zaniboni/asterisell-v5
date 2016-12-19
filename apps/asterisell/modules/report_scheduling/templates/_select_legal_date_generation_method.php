<?php
/**
 * @var ArReportScheduler $ArReportScheduler
 */

$c = new Criteria();
$c->addAscendingOrderByColumn(ArLegalDateGenerationMethodPeer::ID);
$methods = ArLegalDateGenerationMethodPeer::doSelect($c);

$options = array('' => '');
foreach ($methods as $m) {
    /**
     * @var ArLegalDateGenerationMethod $m
     */
    $options[$m->getId()] = $m->getName();
}
$defaultChoice = "";
if (!is_null($ArReportScheduler->getArLegalDateGenerationMethodId())) {
    $defaultChoice = $ArReportScheduler->getArLegalDateGenerationMethodId();
}

echo select_tag('select_legal_date_generation_method', options_for_select($options, $defaultChoice));


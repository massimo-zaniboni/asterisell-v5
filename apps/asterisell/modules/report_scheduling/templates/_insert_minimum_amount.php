<?php

/**
 * @var ArReportScheduler $ArReportScheduler
 */

$value = $ArReportScheduler->getMinimumCost();
if (!is_null($value)) {
    $value = from_db_decimal_to_php_decimal($value);
}

echo input_tag('insert_minimum_amount', $value);
?>
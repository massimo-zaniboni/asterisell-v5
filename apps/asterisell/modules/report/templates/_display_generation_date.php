<?php
use_helper('Asterisell');

/**
 * @var ArReport $ArReport
 */

$d = $ArReport->getProducedReportGenerationDate();
if (!is_null($d)) {
    echo format_date_according_config($d);
} else {
    echo __("None");
}
?>
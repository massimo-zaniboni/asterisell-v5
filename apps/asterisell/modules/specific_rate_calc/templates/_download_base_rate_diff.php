<?php

use_helper('Url', 'I18N');

/**
 * @var ArSpecificRateCalc $ar_specific_rate_calc
 */
$id = $ar_specific_rate_calc->getId();

echo link_to(__('Download CSV file with differences'), 'specific_rate_calc/download_base_rate_diffs?id=' . $id);

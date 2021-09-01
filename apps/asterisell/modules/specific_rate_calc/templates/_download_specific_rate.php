<?php

use_helper('Url', 'I18N');

/**
 * @var ArSpecificRateCalc $ar_specific_rate_calc
 */
$id = $ar_specific_rate_calc->getId();

echo link_to(__('Download rate'), 'specific_rate_calc/download?id=' . $id);

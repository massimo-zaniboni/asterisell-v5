<?php

use_helper('Url', 'I18N');

/**
 * @var ArRate $ar_rate
 */
$rateId = $ar_rate->getId();

echo link_to(__('download file'), 'rate/downloadbackup?id=' . $rateId);

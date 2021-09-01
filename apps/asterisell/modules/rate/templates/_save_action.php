<?php

use_helper('Url', 'I18N');

/**
 * @var ArRate $ar_rate
 */
$rateId = $ar_rate->getId();

echo submit_tag(__('Fix Current Rate'), array('name' => 'save_rate'));
echo '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;' . link_to(__('Download previous overwritten version.'), 'rate/downloadbackup?id=' . $rateId);
?>
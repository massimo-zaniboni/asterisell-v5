<?php
use_helper('Asterisell', 'Url');

/**
 * @var ArWholesaleNumberTransaction $ar_wholesale_number_transaction
 */

$bm = 'wholesale_number_transaction/exportToCsv';
$bmu = '\'' . url_for($bm) . '?at=' . fromMySQLTimestampToUnixTimestamp($ar_wholesale_number_transaction->getFromDate());
$bmu0 = $bmu . '&status=0\';"';
$bmu1 = $bmu . '&status=1\';"';

echo '<ul class="sf_admin_actions">';
  echo '<li><input class="sf_admin_action_reset_filter" value="only this transaction" type="button" onclick="document.location.href=' . $bmu0 . '"/></li>';
  echo '<li><input class="sf_admin_action_reset_filter" value="complete history until this date" type="button" onclick="document.location.href=' . $bmu1 . '"/></li>';
echo '</ul>';

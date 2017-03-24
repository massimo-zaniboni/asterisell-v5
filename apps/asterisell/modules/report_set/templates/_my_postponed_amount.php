<?php
use_helper('Asterisell');
$v = $ar_report_set->getPostponedAmount();
if (is_null($v)) {
} else {
  echo format_from_db_decimal_to_currency_locale($v);
}
?>

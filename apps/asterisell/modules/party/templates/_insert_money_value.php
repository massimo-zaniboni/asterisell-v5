<?php
$value = $ar_party->getMaxLimit30();
if (is_null($value)) {
} else {
  $value = from_db_decimal_to_php_decimal($value);
}
echo input_tag('insert_money_value', $value);
?>
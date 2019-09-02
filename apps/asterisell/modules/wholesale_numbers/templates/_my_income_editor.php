<?php
use_helper('Asterisell');

/**
 * @var ArWholesaleNumber $ar_wholesale_number
 */

$priceDB = $ar_wholesale_number->getIncomePrice();

if (is_null($priceDB)) {
    $priceL = '0';
} else {
    $priceL = from_db_decimal_to_locale_decimal($priceDB);
}

echo input_tag('my_income_editor', $priceL);

?>

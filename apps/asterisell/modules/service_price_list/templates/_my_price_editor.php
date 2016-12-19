<?php
use_helper('Asterisell');

/**
 * @var ArServicePrice $ar_service_price
 */

$priceDB = $ar_service_price->getPrice();

if (is_null($priceDB)) {
    $priceL = '0';
} else {
    $priceL = from_db_decimal_to_locale_decimal($priceDB);
}

echo input_tag('my_price_editor', $priceL);

?>

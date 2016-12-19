<?php
use_helper('Asterisell');

/**
 * @var ArServicePrice $ar_service_price
 */

$price = $ar_service_price->getPrice();

if (is_null($price)) {
} else {
  echo format_from_db_decimal_to_currency_locale($price);
}
?>

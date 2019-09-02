<?php
use_helper('Asterisell');

/**
 * @var ArWholesaleNumber $ar_wholesale_number
 */

echo format_from_db_decimal_to_currency_locale($ar_wholesale_number->getIncomePrice());


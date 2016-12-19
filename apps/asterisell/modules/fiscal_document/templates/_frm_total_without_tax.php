<?php
use_helper('Asterisell');

/**
 * @var ArReport $ArReport
 */

echo format_from_db_decimal_to_currency_locale($ArReport->getTotalWithoutTax());


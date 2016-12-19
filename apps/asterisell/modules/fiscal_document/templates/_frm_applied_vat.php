<?php
use_helper('Asterisell');

/**
 * @var ArReport $ArReport
 */

echo from_db_decimal_to_vat_perc_according_culture($ArReport->getAppliedVat());
echo '%';

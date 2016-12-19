<?php
/**
 * @var ArCdr $ar_cdr
 */
$t = $ar_cdr->getErrorDestinationType();
echo DestinationType::getUntraslatedName($t, false);

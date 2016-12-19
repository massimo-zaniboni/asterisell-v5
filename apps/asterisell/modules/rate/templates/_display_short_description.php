<?php

use_helper('Asterisell');

/**
 * @var ArRate $ar_rate
 */

$phpRate = $ar_rate->unserializePhpRateMethod();

$d = '';
if (is_null($phpRate)) {
    $d = __("No associated rate method.");
} else {
    $d = $phpRate->getShortDescription($ar_rate);
}

$note = $ar_rate->getNote();
if (!isEmptyOrNull($note)) {
    $d .= ' NOTE: ' . $note;
}

echo $d;


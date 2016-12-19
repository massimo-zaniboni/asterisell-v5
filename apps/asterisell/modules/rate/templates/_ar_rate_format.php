<?php

/**
 * @var ArRate $ar_rate
 */
$rateFormat = $ar_rate->getArRateFormat();
if (is_null($rateFormat)) {
    echo "unspecified";
} else {
    echo $rateFormat->__toString();
}
?>
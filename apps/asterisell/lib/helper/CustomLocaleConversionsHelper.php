<?php

// SPDX-License-Identifier: GPL-3.0-or-later

// Custom functions for converting piece of information from a locale to another.
// The default locale is always English.

sfLoader::loadHelpers(array('Asterisell'));

/**
 * @return bool
 */
function isEnglishLanguage() {

    static $isEnglishLanguage = null;

    if (is_null($isEnglishLanguage)) {
        $culture = getCulture();
        if (substr($culture, 0, 2) == 'en') {
            $isEnglishLanguage = true;
        }  else {
            $isEnglishLanguage =  false;
        }
    }

    return $isEnglishLanguage;
}

/**
 * @param bool|null $filterOnRedirectedCalls null for no filter, true for filtering on redirected calls, false for filtering on explicitely non redirected calls
 * @param int|null $destinationType the direction of the call
 * @return string a message with the applied filter to use in the UI (no in the reports).
 */
function translateFilterOnCalls($filterOnRedirectedCalls, $destinationType) {

    if (!is_null($filterOnRedirectedCalls)) {
        if ($filterOnRedirectedCalls) {
            $v1 = __(' redirected ');
        } else {
            $v1 = __(' no redirected ');
        }
    } else {
        $v1 = '';
    }

    if (is_null($destinationType)) {
        $v2 = '';
    } else {
        $v2 = DestinationType::getName($destinationType, false);
    }

    if (getCulture() == 'it_IT') {
        return 'Tutte le chiamate ' . $v1 . ' ' . $v2 . ' ';
    } else {
        return __('All ') . $v1 . ' ' . $v2 . ' ' . __('calls ');
    }
}

/**
 * Produce a 3 line header, for a call report, describing the type of filtered calls.
 * @param string $reportShortDescription already converted
 * @param int[] $directions
 * @param string $organization
 * @param string|null $vendorName null for all vendors
 * @param int $fromDate
 * @param int $toDate
 * @return string
 */
function translateFilterOnReportHedaer($reportShortDescription, $directions, $organization,  $vendorName, $fromDate, $toDate) {
    $s = array();
    foreach($directions as $d) {
        $s[] = DestinationType::getName($d, false);
    }

    if (count($s) > 0) {
        $directionS1 = $directionS = implode(', ', $s);
        if (getCulture() == 'it_IT') {
            $directionS = ' delle chiamate ' . $directionS1 . ' ';
        } else {
            $directionS = ' for ' . $directionS1 . ' calls ';
        }
    } else {
        $directionS = '';
    }

    if (!isEmptyOrNull($vendorName)) {
        $vendorS = mytr(', restricted to vendor ') . $vendorName;
    } else {
        $vendorS = '';
    }

    $timeFrameS = getUserReadableTimeFrame($fromDate, $toDate, true, true);

    if (getCulture() == 'it_IT') {
        return $reportShortDescription . $directionS . $vendorS . "\ndi " . $organization . "\n" . $timeFrameS;
    } else {
        return $reportShortDescription . $vendorS . "\nof " . $organization . "\n" . $timeFrameS;
    }
}

?>
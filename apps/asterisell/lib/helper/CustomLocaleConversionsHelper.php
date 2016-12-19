<?php

/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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
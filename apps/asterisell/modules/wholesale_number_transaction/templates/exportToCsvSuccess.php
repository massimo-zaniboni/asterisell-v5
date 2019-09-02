<?php

use_helper('I18N', 'Debug', 'Date', 'Asterisell');

// Set UTF-8 encoding
echo "\xEF\xBB\xBF";

echo csv_field('Effect', true);
echo csv_field('From timestamp', false);
echo csv_field('Last date comment', false);
echo csv_field('Action comment', false);
echo csv_field('Reseller', false);
echo csv_field('Carrier', false);
echo csv_field('Telephone number', false);
echo csv_field('Cost', false);
echo csv_field('Income', false);
echo csv_field('Extension codes', false);
echo "\n";

if (VariableFrame::$wholesaleStatus == 5) {
    echo csv_field(VariableFrame::$wholesaleEffect, true);
    echo csv_field(fromUnixTimestampToMySQLTimestamp(VariableFrame::$wholeFromDate), false);
    echo csv_field('not important', false);
    echo csv_field('not important', false);
    echo csv_field('complete with a reseller code', false);
    echo csv_field('complete with a carrier code', false);
    echo csv_field('complete with a telephone number', false);
    echo csv_field('0.00', false);
    echo csv_field('0.00', false);
    echo csv_field('', false);
    echo "\n";
} else {
    $cFromDate = null;

    $isFirst = true;
    $stm = Propel::getConnection()->prepare(VariableFrame::$wholeQuery);
    $stm->execute(VariableFrame::$listParams);
    while ($r = $stm->fetch(PDO::FETCH_ASSOC)) {

        if ($isFirst) {
            echo csv_field(VariableFrame::$wholesaleEffect, true);
        } else {
            echo csv_field('', true);
        }

        if (VariableFrame::$wholesaleStatus == 2) {
            if ($isFirst) {
                echo csv_field(fromUnixTimestampToMySQLTimestamp(VariableFrame::$wholeFromDate), false);
            } else {
                echo csv_field('', false);
            }
        } else {
            $v = $r['from_date'];
            if ($cFromDate == $v) {
                echo csv_field('', false);
            } else {
                echo csv_field($v, false);
                $cFromDate = $v;
            }
        }

        if (VariableFrame::$wholesaleStatus == 2) {
            echo csv_field('', false);
        } else {
            $v = $r['csv_last_date'];
            if (is_null($v)) {
                echo csv_field('', false);
            } else {
                echo csv_field($v, false);
            }
        }

        if (VariableFrame::$wholesaleStatus == 2) {
            echo csv_field('', false);
        } else {
            echo csv_field($r['csv_comment'], false);
        }

        $v = $r['reseller_code'];
        if ($r['exists'] == 0) {
            echo csv_field(ArWholesaleNumber::DELETE_SPECIAL_CODE, false);
        } else if (is_null($v)) {
            echo csv_field(ArWholesaleNumber::FREE_SPECIAL_CODE, false);
        } else {
            echo csv_field($v, false);
        }

        echo csv_field($r['carrier_code'], false);

        echo csv_field($r['telephone_number'], false);

        $v = from_db_decimal_to_smart_php_decimal($r['cost']);
        echo csv_field($v, false);

        $v = from_db_decimal_to_smart_php_decimal($r['income']);
        echo csv_field($v, false);

        if ($r['use_default_extension_codes'] == 1) {
            echo csv_field('', false);
        } else {
            echo csv_field($r['extension_codes'], false);
        }
        echo "\n";

        $isFirst = false;
    }
    $stm->closeCursor();
}

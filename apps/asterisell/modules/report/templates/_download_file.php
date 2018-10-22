<?php

use_helper('Url', 'I18N');

if ($ArReport->isThereDocument()) {

    if ($ArReport->mustBeRegenerated()) {
        $s1 = 'This report was generated before a re-rating of calls, so it is not anymore updated. ';
        $s2 = 'Download the report in any case.';
    } else {
        $s1 = '';
        $s2 = __('Download Report');
    }

    echo $s1 . link_to($s2, 'report/download?id=' . $ArReport->getId());
} else {
    echo __('no generated report');
}

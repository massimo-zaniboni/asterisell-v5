<?php

use_helper('Url', 'I18N');

if ($ArReport->isThereDocument()) {
    echo link_to(__('download report'), 'report/download?id=' . $ArReport->getId());
} else {
    echo __('no generated report');
}

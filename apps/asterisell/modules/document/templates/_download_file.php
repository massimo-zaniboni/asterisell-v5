<?php

use_helper('Url', 'I18N');

if ($ar_document->isThereAttachedDocument()) {
    echo link_to(__('download document'), 'document/download?id=' . $ar_document->getId());
} else {
    echo __('no attached document');
}

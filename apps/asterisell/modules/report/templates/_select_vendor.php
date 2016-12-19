<?php

/**
 * @var ArReport $ArReport
 */

$c = new Criteria();
$vendors = ArVendorPeer::doSelect($c);

$options = array();
$options[''] = '';
foreach($vendors as $vendor) {
    /**
     * @var ArVendor $vendor
     */
    $options[$vendor->getId()] = $vendor->getName();
}

asort($options, SORT_LOCALE_STRING);

$index = $ArReport->getArVendorId();
if (is_null($index)) {
    $index = '';
}

echo select_tag('select_vendor', options_for_select($options, $index));

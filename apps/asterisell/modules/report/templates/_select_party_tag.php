<?php

/**
 * @var ArReport $ArReport
 */

$c = new Criteria();
$vv = ArTagPeer::doSelect($c);

$options = array();
$options[''] = '';
foreach($vv as $v) {
    /**
     * @var ArTag $v
     */
    $options[$v->getId()] = $v->getInternalName();
}

asort($options, SORT_LOCALE_STRING);

$index = $ArReport->getArTagId();
if (is_null($index)) {
    $index = '';
}

echo select_tag('select_party_tag', options_for_select($options, $index));

<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

// Show from for importing CSV

$base = sfConfig::get('app_instance_url_path');
if ($base !== '') {
  if (substr($base, 0, 1) !== '/') {
    $base = '/' . $base;
  }
}
$helpPage = 'wholesale_numbers';

$c = new Criteria();
$vs = ArResellerPeer::doSelect($c);
$selectReseller = array();
$selectReseller[''] = '';
foreach($vs as $v) {
    /**
     * @var ArReseller $v
     */
    $selectReseller[$v->getId()] = $v->getInternalName() . ' - ' . $v->getName();
}
asort($selectReseller, SORT_LOCALE_STRING);

// Show buttons in a table

echo form_tag("wholesale_number_transaction/exportToCsv", 'multipart=true');
echo '<table class="sf_admin_list">';
echo '<tbody>';
echo '<tr class="sf_admin_row_0">';

echo '<td style="text-align:center;">';
echo submit_tag('Create new numbers', array('name' => 'downloadNewNumbers'));
echo '</th>';

echo '<td style="text-align:center;">';
echo submit_tag('Download/assign free numbers', array('name' => 'downloadFreeNumbers'));
echo '</th>';

echo '<td style="text-align:center;">';
echo submit_tag('Download only active numbers', array('name' => 'downloadActiveNumbers'));
echo '</th>';

echo '<td style="text-align:center;">';
echo select_tag('select_reseller', options_for_select($selectReseller, ''));
echo '      ';
echo submit_tag('Download numbers assigned to this reseller', array('name' => 'downloadResellerNumbers'));
echo '</td>';

echo '<td style="text-align:center;">';
echo input_file_tag('csvFile', array('size' => 30));
echo '      ';
echo submit_tag('Import data from this CSV file', array('name' => 'importCsv'));
echo '      ';
echo ' <a style="margin-left: 10px" href="' . $base . '/admin/manual/' . $helpPage . '.html" target="_blank">' . __('Info about CSV format') . '</a>';
echo '</td>';

echo '</tr></tbody></table>';
echo '</form>';


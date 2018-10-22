<?php

$conn = Propel::getConnection();
$q = 'SELECT DISTINCT internal_name FROM ar_rate ORDER BY internal_name';
$stmt = $conn->prepare($q);
$stmt->execute(array());

$options = array();
$options[''] = '';
while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
  $n = $rs[0];
  $options[$n] = $n;
}
$stmt->closeCursor();

$defaultChoice = "";
if (isset($filters['filter_on_name_list'])) {
    $defaultChoice = $filters['filter_on_name_list'];
}

echo select_tag('filters[filter_on_name_list]', options_for_select($options, $defaultChoice));

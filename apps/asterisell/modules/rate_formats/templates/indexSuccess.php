<?php

echo '<div id="asterisellHelp">';

//
// Display the Initial Index with all the Rate Formats
//

$stm = Propel::getConnection()->prepare('SELECT short_description, internal_name FROM ar_rate_format ORDER BY order_name');
$stm->execute();

echo 'Supported rate configurations, and data, formats are:<ul>';

while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
  echo '<li><a href="#' . $rs[1] . '">' . htmlspecialchars('(' . $rs[1] . ') ' . $rs[0]) . '</a>' . "\n";
}
$stm->closeCursor();

echo '</ul>';

//
// Display Details for each rate format
//

$stm = Propel::getConnection()->prepare('SELECT short_description, internal_name, detailed_description FROM ar_rate_format ORDER BY order_name');
$stm->execute();

while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
  echo '<a id="' . $rs[1] . '"></a>';
  echo '<h2>' . htmlspecialchars('(' . $rs[1] . ') ' . $rs[0]) . '</h2>';
  echo $rs[2];
}
$stm->closeCursor();

?>

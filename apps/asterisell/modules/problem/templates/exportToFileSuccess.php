<?php

use_helper('I18N', 'Debug', 'Date', 'Asterisell');

$conn = Propel::getConnection();
$stmt = $conn->prepare('
          SELECT
             t.name
          ,  d.name
          ,  r.name
          ,  p.created_at
          ,  p.description
          ,  p.effect
          ,  p.proposed_solution
          ,  p.count_of_cdrs
          ,  p.duplication_key
          ,  p.garbage_collection_key
          FROM ar_current_problem AS p
          INNER JOIN ar_problem_type AS t
          ON p.ar_problem_type_id = t.id
          INNER JOIN ar_problem_domain AS d
          ON p.ar_problem_domain_id = d.id
          INNER JOIN ar_problem_responsible AS r
          ON p.ar_problem_responsible_id = r.id
          ORDER BY
              p.ar_problem_type_id
            , p.ar_problem_domain_id
            , p.ar_problem_responsible_id
            , p.count_of_cdrs DESC
            ;
          ');

// Set UTF-8 encoding
echo "\xEF\xBB\xBF";

$stmt->execute(array());
while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
  echo "\n\n# ---------------------------------------";
  echo "\nproblem_key: " . $rs[8];
  echo "\ncreation_date: " . $rs[3];
  echo "\ntype: " . $rs[0];
  echo "\ndomain: " . $rs[1];
  echo "\nresponsible: " . $rs[2];
  echo "\naffected_cdrs: ";
  if (is_null($rs[7])) {
      echo "-";
  } else {
      echo $rs[7];
  }
  echo "\ntested_every_time:";
  if (is_null($rs[9])) {
      echo "no";
  } else {
      echo "yes";
  }
  echo "\ndescription: " . $rs[4];
  echo "\neffect: " . $rs[5];
  echo "\nproposed_solution: " . $rs[6];
}
$stmt->closeCursor();

<?php
/**
 * @var ArCdr $ar_cdr
 */

$p = $ar_cdr->getArCurrentProblem();
if (is_null($p)) {
    echo '';
} else {
  $id = $p->getArProblemResponsibleId();
  echo ArProblemResponsible::getTypeName($id);
}
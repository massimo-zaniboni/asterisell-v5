<?php
/**
 * @var ArCdr $ar_cdr
 */

$p = $ar_cdr->getArCurrentProblem();
if (is_null($p)) {
    echo '';
} else {
    $id = $p->getArProblemTypeId();
    echo ArProblemType::getTypeName($id);
}

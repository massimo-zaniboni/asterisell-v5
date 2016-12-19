<?php
/**
 * @var ArCurrentProblem $ar_current_problem
 */

$id = $ar_current_problem->getArProblemResponsibleId();
echo ArProblemResponsible::getTypeName($id);

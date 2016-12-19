<?php
/**
 * @var ArCurrentProblem $ar_current_problem
 */

$id = $ar_current_problem->getArProblemTypeId();
echo ArProblemType::getTypeName($id);

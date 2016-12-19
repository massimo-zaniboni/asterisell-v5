<?php
/**
 * @var ArCurrentProblem $ar_current_problem
 */

$id = $ar_current_problem->getArProblemDomainId();
echo ArProblemDomain::getTypeName($id);

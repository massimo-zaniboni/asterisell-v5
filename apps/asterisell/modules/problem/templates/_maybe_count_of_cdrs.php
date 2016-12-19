<?php
use_helper('Form');

/**
 * @var ArCurrentProblem $ar_current_problem
 */

$c = $ar_current_problem->getCountOfCdrs();

if ($c == 0) {
    echo '-';
} else {
   echo $c;
}

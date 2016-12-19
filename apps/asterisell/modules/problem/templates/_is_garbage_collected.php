<?php
use_helper('Form');

/**
 * @var ArCurrentProblem $ar_current_problem
 */

$key = $ar_current_problem->getGarbageCollectionKey();

if (is_null($key)) {
    echo 'no';
} else {
   echo 'yes';
}



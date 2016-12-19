<?php
/**
 * @var ArCdr $ar_cdr
 */

$p = $ar_cdr->getArCurrentProblem();
if (is_null($p)) {
    echo "The problem related to this CDR was deleted from the table of problems.\nFor generating again the error description, you must force a re-rating of the time-frame containing this CDR.";
} else {
    $value = object_textarea_tag($p, 'getDescription', array(
        'control_name' => 'ar_current_problem[description]',
        'size' => '70x10',
    ));

    echo $value ? $value : '&nbsp;';
}
?>

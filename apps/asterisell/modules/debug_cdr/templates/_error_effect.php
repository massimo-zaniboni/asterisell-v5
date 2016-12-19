<?php
/**
 * @var ArCdr $ar_cdr
 */

$p = $ar_cdr->getArCurrentProblem();
if (is_null($p)) {
    echo '';
} else {

    $value = object_textarea_tag($p, 'getEffect', array(
        'control_name' => 'ar_current_problem[effect]',
        'size' => '70x10',
    ));

    echo $value ? $value : '&nbsp;';
}
?>

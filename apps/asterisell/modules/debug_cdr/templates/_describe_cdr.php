<?php
/**
 * @var ArCdr $ar_cdr
 */

$value = object_textarea_tag($ar_cdr, 'getDebugDescription', array (
  'control_name' => 'ar_current_problem[cdr_debug]',
  'size' => '70x22',
));

echo $value ? $value : '&nbsp;'

?>

<?php 

require 'generator_header.php';

$moduleName = FieldsToShow::getModuleName($generateForAdmin);

echo '<?php';

?>

use_helper('I18N', 'Date');

use_stylesheet('/sf/sf_admin/css/main');



echo '<div id="sf_admin_container">';

echo '<div id="sf_admin_header">';

include_partial(<?php echo "'$moduleName/list_header'" ?>);
include_partial(<?php echo "'$moduleName/list_messages'" ?>);

echo '</div>';

echo '<div id="sf_admin_bar">';

include_partial('filters', array('filters' => $filters));

echo '</div>';

echo '<div id="sf_admin_content">';

include_partial(<?php echo "'$moduleName/list'" ?>);

echo '</div>';

echo '<div id="sf_admin_footer">';

include_partial(<?php echo "'$moduleName/list_footer'"?>);

echo '</div>';
echo '</div>';


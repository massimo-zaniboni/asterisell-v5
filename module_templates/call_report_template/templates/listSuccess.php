<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

echo '</div>';
echo '</div>';


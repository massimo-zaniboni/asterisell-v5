<?php

/**
 * This file is included from files like `_list.php`, and include common logic
 * used for generating the final `_list.php` to install in production.
 */

require_once(dirname(__FILE__) . '/../../config/ProjectConfiguration.class.php');
$configuration = ProjectConfiguration::getApplicationConfiguration('asterisell', 'prod', false);
sfContext::createInstance($configuration);

$databaseManager = new sfDatabaseManager($configuration);
$databaseManager->loadConfiguration();

sfLoader::loadHelpers('I18N', 'Debug', 'Asterisell', 'CustomLocaleConversions');

if ($argv[1] == "customer") {
    $generateForAdmin = false;
} else if ($argv[1] == "admin") {
    $generateForAdmin = true;
} else {
    die("Invalid first parameter. Use \"customer\" or \"admin\"");
}

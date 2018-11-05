<?php


// SPDX-License-Identifier: GPL-3.0-or-later

////////////////////////////////////
// INIT ENVIRONMENT AND CALL MAIN //
////////////////////////////////////

//
// Manage Debug Mode and other activation flags.
//

/**
 * Set usually to false.
 * Set to true in case of run-time errors that must be intercepted.
 */
$debugMode = false;

/**
 * @param string $script
 * @param string $line
 * @param string $message
 */
function assert_callback($script, $line, $message)
{
    echo 'You have a design error in your script ', $script, ': line ' . $line;
    echo 'message: ' . $message;
    exit(1);
}

assert_options(ASSERT_ACTIVE, true);
assert_options(ASSERT_BAIL, true);
assert_options(ASSERT_WARNING, true);
assert_options(ASSERT_QUIET_EVAL, false);
assert_options(ASSERT_CALLBACK, 'assert_callback');

require_once(dirname(__FILE__) . '/config/ProjectConfiguration.class.php');

if ($debugMode == true || ($argc > 1 && trim($argv[1]) === "debug")) {
    if ($debugMode) {
        echo "\nWARNING: debug mode activate.";
    }
    $configuration = ProjectConfiguration::getApplicationConfiguration('asterisell', 'dev', true);
} else {
    $configuration = ProjectConfiguration::getApplicationConfiguration('asterisell', 'prod', false);
}
sfContext::createInstance($configuration);

// An external job must set explicitely the culture, because it isn't running
// inside a request.
$i18nDir = dirname(getAsterisellCompleteRootDirectory()) . '/admin/apps/asterisell/i18n';
$culture = trim(sfConfig::get('app_culture'));
$I18N = sfContext::getInstance()->getI18N();
$i18nConversions = array($i18nDir);
$I18N->setMessageSource($i18nConversions, $culture);
$I18N->setCulture($culture);

$databaseManager = new sfDatabaseManager($configuration);
$databaseManager->loadConfiguration();

/**
 * Special file where read user input.
 */
$input_line = fopen('php://stdin', 'r');

$exitCode = main($argc, $argv);
exit($exitCode);

/**
 * @return void
 */
function displayUsage()
{
    $str = <<<HELP
##
## Installation/development operations
##

  php asterisell.php activate

      Clear cache, set directories, and other common and safe management operations.

  php asterisell.php activate users [db-root-user] [db-root-password]

      Create again the MySQL users for accessing the application.
      NOTE: if a new name is used, then old users had to be deleted manually.

  php asterisell.php make-derived-files

      Internal command.

  php asterisell.php silently-activate

      Internal command.

  php asterisell.php install

      Initial install with empty database.

  php asterisell.php install-demo

      To call after "install" command for installing demo data.

  php asterisell.php install-views-and-procedures

      Install views, procedures, and calls rating procedures, without deleting data. Useful in case of restore from an instance with errors, or for updating of the code.

  php asterisell.php run [upgrade-jobs|db-upgrade-jobs]

      Execute only upgrading jobs. Called from the administrator.

  php asterisell.php data admin <some-password>

      Add an "admin" user, with the specified password.

  php asterisell.php data unbilled YY-MM-DD hh:mm:s

      All CDRS befor this calldate are considered as already billed.

  php asterisell.php data merge-telephone-prefixes

      Add new telephone prefixes to the telephone prefix table, reading them from "scripts/world_prefix_table.csv".

  php asterisell.php dev remove-model <SomeModelClassName>

      Remove from lib directory, the occurrences of the model.

  php asterisell.php dev update-customizations

      Update the description of rate formats, and other settings that are likely to be changed during development.

  php asterisell.php dev support-user <password>

      Create or modify an admin support user, with login "support", and the specified password.

  php asterisell.php debug stress-rerating [MAX-DAYS-IN-THE-PAST] [HOURS]

      Rerate starting from the specified days in the past, simulating new CDRS every specified hours, using random time-frames, and checking for errors in grouped cached CDRS.

  php asterisell.php debug test <test-class> <test-method>

      Called from regression test code for executing certain regression tests.

  php asterisell.php debug some-code-test

      Start some demo code, useful during development.

  php asterisell.php debug regression-test <db-root-user> <db-root-password>

      Execute unit tests on code.

  php asterisell.php debug signal-critical-problem-in-the-code

      Signal in the problem table, a critical problem in the code.

  php asterisell.php run scheduled-jobs

      Start job processor in silent mode, usually called from cron daemon.

  php asterisell.php data update-cached-cdrs

      Recalculate all daily totals, of the table ar_cached_cdrs, according the new content of ar_cdr.
      Used only during development or maintenance, if some unusual operations corrupt the content of the table.

##
## Maintenance operations
##

  php asterisell.php cron [enable|disable|disable-for-upgrade]

      Enable or disable the cron job processor: the application can be used for viewing old data, but not process new data

  php asterisell.php app [enable|disable]

      Disable application access for normal users, saying that the application is under maintenance.

  php asterisell.php dev list-jobs

      List scheduled jobs with related development notes.

  php asterisell.php cron force-execution-of-all-jobs

      At next job execution also postponed (maintenance) jobs will be executed.

  php asterisell.php run jobs

      Start job processor. Called from the administrator.

  php asterisell.php debug jobs

      Start job processor reporting problems on the command line and in the log/asterisell_dev.log file, and executing the rating engine in debug/test mode.
      This mode is a lot slower, but it can detect more errors in the code.

  php asterisell.php debug rerate [yyyy-mm-dd]

      Schedule a rerate event. The CDRs will be rated at next execution of jobs.
      If none calldate is specified, then all not yet billed CDRS are scheduled for rerate.

  php asterisell.php debug rate [yyyy-mm-dd]

      Rate immediately the CDRs from the specified calldate.

  php asterisell.php debug debug-rate [yyyy-mm-dd]

      Rate immediately the CDRs from the specified calldate, generating debug info.
      NOTE: more accurate info is generated, but it is slower than normal rating.

  php asterisell.php debug reset-rerate-event

      Reset the scheduled rerate events.

##
## Data operations
##

  php asterisell.php data backup

      Create a file with the dump of the database.

  php asterisell.php data config-backup

      Make a compact backup of the database, excluding CDRs data.

  php asterisell.php data restore

      Restore the application data, according the content of "data_files/messages/backup" directory.
      This is the suggested way for restoring data.
      After the execution of this command the content of the backup directory will be overwritten with new data.

  php asterisell.php data export-organizations <file-name>

      Export organizations to file-name.

  php asterisell.php data import-organizations <file-name> [new]

      Import organizations from an YAML file.
      Specify "new" for considering all data as new data, also if the part-id is specified.

  php asterisell.php data complete-reseller-export-code <reseller-code>

      Given the code of a reseller, complete the export-code field of the children extensions, with the first value in extensions codes. Only extensions with an empty value are affected.
      This is a sane default value, that can be used for extensions to export to resellers, in case a batch initialization is needed. These values can be specified individually also in the user interface.

  php asterisell.php data delete-organization <id>

      Given the id of an organization (the unique identifier in a URL like "view/id/123"), delete from the database. Useful in case of infinite loops in definition of an organization, and other bad configurations.

  php asterisell.php data export-cdrs [cdr-provider-code] [cdr-format] [yyyy-mm-[dd]]

      Without params, list the available cdr-provider-codes and cdr-format.
      Use YYYY-MM for exporting all the CDRs of a month, and YYYY-MM-DD for exporting all the CDRs of a day.


HELP;

    echo $str;
    showMaintananceMode();
}

////////////////////
// DATABASE TESTS //
////////////////////

/**
 * @return bool TRUE if there is a safe connection to the database
 */
function isSafeReadConnection()
{
    try {
        $connection = Propel::getConnection();
        $query = 'SELECT id FROM ar_cdr LIMIT 1';

        $stm = $connection->prepare($query);
        $stm->execute();

        while ($stm->fetchColumn()) {
        }
        $stm->closeCursor();
        return TRUE;
    } catch (Exception $e) {
        return FALSE;
    }
}

/////////////////////
// DELETE DATABASE //
/////////////////////

/**
 * @param PDO $c
 * @param string $t
 * @return void
 */
function myDelete($c, $t)
{
    $query = "DELETE FROM $t";
    $stmt = $c->prepare($query);
    $stmt->execute();
}

////////////////////////////////////
// APPLICATION MANAGEMENT SECTION //
////////////////////////////////////

/**
 * @param string $password
 * @return void
 */
function addRootUser($password)
{
    $defaultParamsId = ConfigureDefaultParamsAndSettings::getOrCreateDefaultParams();

    $user = ArUserPeer::getRootUser();

    if (!is_null($user)) {
        $party = $user->getArParty();
        if (!is_null($party)) {
            $party->delete();
        }
        $user->delete();
    }

    $p = new ArParty();
    $p->setName('admin');
    $p->setIsActive(true);
    $p->save();

    $w = new ArUser();
    $w->setLogin("admin");
    $w->setClearPassword($password);
    $w->setIsEnabled(true);
    $w->setIsRootAdmin(true);
    $w->setArParty($p);
    $w->save();

    // NOTE: additional roles and permissions to root user are added after permissions and roles
    // are created.

    echo "\nCreated \"admin\" user with password \"$password\".\n";
}


/**
 * Exit if the user does not confirm.
 *
 * @return void
 */
function explicitConfirmForDeletion($isInteractive = TRUE)
{
    global $input_line;

    if ($isInteractive) {
        list($database, $user, $password) = getDatabaseNameUserAndPassword();

        echo "\nWARNING: all data in database '$database' will be deleted. Are you sure? [y/N]";

        $next_line = trim(fgets($input_line, 1024));
        if ($next_line === "y" || $next_line === "Y") {
        } else {
            echo "\nStop execution: database was not modified.\n";
            exit(1);
        }
    }
}

/**
 * @param string $comment
 * @param string $cmd
 */
function myExecute($comment, $cmd)
{
    echo "\n$comment";
    echo "\nexecute> $cmd\n";
    system($cmd, $result);
}

/**
 * @return void
 */
function explicitContinue()
{
    global $input_line;

    echo "\nPress Enter to continue...";
    echo "\n";

    trim(fgets($input_line, 1024));
}

/**
 * Create an user.
 * @param string $rootUser
 * @param string $rootPassword
 * @param string $database
 * @param string $user
 * @param string $password
 * @param bool $isAdmin true if the created user is an admin user of the application,
 * false if it is a normal customer.
 */
function createMySQLUser($rootUser, $rootPassword, $database, $user, $password, $isAdmin)
{
    myExecute("Create database user '$user'", "mysql -u $rootUser --password=$rootPassword mysql -e \"CREATE USER IF NOT EXISTS $user@localhost IDENTIFIED BY '$password';\" ");
    myExecute("Change $user password in case the user was already existing ", "mysql -u $rootUser --password=$rootPassword mysql -e \"ALTER USER $user@localhost IDENTIFIED BY '$password';\"");

    if ($isAdmin) {
        myExecute("Grant Access", "mysql -u $rootUser --password=$rootPassword mysql -e \"GRANT ALL ON $database.* TO $user@localhost;\"");
        myExecute("Grant Load File Access", "mysql -u $rootUser --password=$rootPassword mysql -e \"GRANT FILE ON *.* TO $user@localhost;\"");
    } else {
        myExecute("Grant Access", "mysql -u $rootUser --password=$rootPassword mysql -e \"GRANT SELECT ON $database.* TO $user@localhost;\"");
        myExecute("Grant Access", "mysql -u $rootUser --password=$rootPassword mysql -e \"GRANT SELECT, UPDATE ON $database.ar_report_to_read TO $user@localhost;\"");
        myExecute("Grant Access", "mysql -u $rootUser --password=$rootPassword mysql -e \"GRANT INSERT ON $database.ar_user_change_password_request TO $user@localhost;\"");
        myExecute("Grant Access", "mysql -u $rootUser --password=$rootPassword mysql -e \"REVOKE SELECT ON $database.ar_user_change_password_request FROM $user@localhost;\"");
    }
}

/**
 * @param string|null $rootUser
 * @param string|null $rootPassword
 * @return void
 */
function makeInstallCreateDatabase($rootUser = null, $rootPassword = null)
{
    global $input_line;

    list($database, $user, $password) = getDatabaseNameUserAndPassword(true);

    $isInteractive = (is_null($rootUser) || is_null($rootPassword));

    if ($isInteractive) {
        echo "\nConfirm you want create database '$database', user '$user', with password '$password', as specified in file 'configure/databases.yms' [y/N]";
        $next_line = trim(fgets($input_line, 1024));
        if ($next_line === "y" || $next_line === "Y") {
        } else {
            echo "\nStop execution: database data was not confirmed.\n";
            exit(1);
        }

        echo "\nEnter the name of MySQL administrator user, or another MySQL user that can create new databases: ";
        $rootUser = trim(fgets($input_line, 1024));

        echo "\nEnter the MySQL password of the administrator user $rootUser:  ";
        $rootPassword = trim(fgets($input_line, 1024));
    }

    myExecute("Drop '$database' database", "mysqladmin -u $rootUser --password=$rootPassword drop --force $database");
    myExecute("Create '$database' database", "mysql -u$rootUser --password=$rootPassword -e 'CREATE DATABASE $database DEFAULT CHARACTER SET utf8mb4 DEFAULT COLLATE utf8mb4_bin;'");
    myExecute("Init '$database' schema", "mysql -u $rootUser --password=$rootPassword $database < data/sql/lib.model.schema.sql");

    makeInstallCreateUsers($rootUser, $rootPassword);
}


/**
 * Create admin and customer users.
 * @param string|null $rootUser
 * @param string|null $rootPassword
 * @return void
 */
function makeInstallCreateUsers($rootUser = null, $rootPassword = null)
{
    global $input_line;

    list($database, $user, $password) = getDatabaseNameUserAndPassword(true);

    $isInteractive = (is_null($rootUser) || is_null($rootPassword));

    if ($isInteractive) {
        echo "\nEnter the name of MySQL administrator user, or another MySQL user that can create new users: ";
        $rootUser = trim(fgets($input_line, 1024));

        echo "\nEnter the MySQL password of the administrator user $rootUser:  ";
        $rootPassword = trim(fgets($input_line, 1024));
    }

    createMySQLUser($rootUser, $rootPassword, $database, $user, $password, true);

    list($customerDatabase, $customerUser, $customerPassword) = getDatabaseNameUserAndPassword(false);
    createMySQLUser($rootUser, $rootPassword, $customerDatabase, $customerUser, $customerPassword, false);

    if ($isInteractive) {
        explicitContinue();
    }
    makeActivate($isInteractive);
}

/**
 * @param bool $isInteractive
 * @param bool $onlyConfig
 * @return string|null the created file, null if no file were created
 */
function makeDatabaseBackup($isInteractive = true, $onlyConfig = false)
{
    global $input_line;

    list($database, $user, $password) = getDatabaseNameUserAndPassword();

    $onlyConfigTables = BackupConfigurations::getConfigurationTablesToIgnoreInOnlyConfigExport();

    $options = ' ';
    if ($onlyConfig) {
        foreach ($onlyConfigTables as $t) {
            $options .= " --ignore-table=$database.$t ";
        }
    }

    $fileName = '/var/tmp/';
    if ($onlyConfig) {
        $fileName .= 'backup-only-config-' . date('Y-m-d_H-i-s') . '.sql';
    } else {
        $fileName .= 'backup-' . date('Y-m-d_H-i-s') . '.sql';
    }
    $fileName .= '.gz';

    $cmd = "mysqldump -u $user --password=$password $database --single-transaction $options | gzip > $fileName";
    $restoreCmd = "pv $fileName | gunzip | mysql -uroot -p" . $password . " " . $database;

    if ($isInteractive) {
        echo "\nThe database can be backuped using the command\n\n   > $cmd";
        echo "\nand restored using the command\n\n   > $restoreCmd \n\n";
    }

    $r = system($cmd);
    if ($r === FALSE) {
        if ($isInteractive) {
            echo "\nError during execution of command";
        }
        return null;
    } else {
        return $fileName;
    }
}

/**
 * @param bool $askPermission true for asking permission before restore
 * @return true|string true if the restore is performed, false if it is aborted, and the error message in case of problems during restore.
 */
function makeDatabaseRestore($askPermission)
{

    global $input_line;

    $makeRestore = FALSE;
    if ($askPermission) {
        echo "\nIf you confirm, all the content of the *current database will be lost*, and replaced with the data in backup database directory. Are you sure? [yes/NO]\n";
        $next_line = trim(fgets($input_line, 1024));
        if ($next_line === "yes" || $next_line === "YES") {
            $makeRestore = TRUE;
        }
    } else {
        $makeRestore = TRUE;
    }

    if ($makeRestore) {
        list($database, $user, $password) = getDatabaseNameUserAndPassword();

        // Drop database, and install an empty copy.
        echo "\n\nStart with an empty database.\n\n";
        manageCommand_install(false, true);

        echo "\n\nLoad configurations.\n\n";
        $confRestore = new BackupConfigurations();
        $errorMsg = $confRestore->restoreFromBackup();
        if (!is_null($errorMsg)) {
            return $errorMsg;
        }

        echo "\n\nLoad reports.\n\n";
        $reportRestore = new BackupReports();
        $errorMsg = $reportRestore->restoreFromBackup();
        if (!is_null($errorMsg)) {
            return $errorMsg;
        }

        echo "\n\nLoad Source CDRs.\n\n";
        $cdrsRestore = new BackupSourceCDRS();
        $errorMsg = $cdrsRestore->restoreFromBackup();
        if (!is_null($errorMsg)) {
            return $errorMsg;
        }

        echo "\n\nLoad CDRs.\n\n";
        $cdrsRestore = new BackupCDRS();
        $errorMsg = $cdrsRestore->restoreFromBackup();
        if (!is_null($errorMsg)) {
            return $errorMsg;
        }

        echo "\n\nUpdate ar_cached_cdrs and ar_cached_errors tables.\n\n";
        if (!RateEngineService::executeUpdateAllCachedCDRS()) {
            return "Error during updating of ar_cached_cdrs and ar_cached_errors table";
        }

        return true;
    } else {
        return false;
    }
}

/**
 * @param string $user
 * @param bool $isAdmin
 */
function makeActivateDirs($user, $isAdmin)
{

    if ($isAdmin) {
        $preCmd = "cd " . getAsterisellCompleteAdminDirectory();
    } else {
        $preCmd = "cd " . getAsterisellCompleteUserDirectory();
    }

    $cmd = $preCmd . ' && chown -R ' . $user . ':' . $user . ' . ';
    myExecute("Fix ownerships", $cmd);

    $webWritableDirs = array(
        'cache/',
        'log/',
        'web/',
        'web/uploads/assets/'
    );

    $makeDirCmd = $preCmd . ' && mkdir -p ';
    $chmodCmd = $preCmd . ' && chmod -R ug+rwx,o-rwx ';
    foreach ($webWritableDirs as $d) {
        $makeDirCmd .= " $d ";
        $chmodCmd .= " $d ";
    }
    myExecute("Create web-server working directories", $makeDirCmd);
    myExecute("COPYRIGHT", $preCmd . ' && chmod g+r COPYRIGHT');

    if (!$isAdmin) {
        executeSymfonyCC($isAdmin);
    }

    myExecute("Fix Permissions for web-server writable directories", $chmodCmd);
    myExecute("Fix Permissions for web-server readable directories", "$preCmd && chmod -R g+rx,g-w,u+rwx,o-rwx ext_libs/ apps/ lib/ config/ data/ symfony symfony.php");
    myExecute("Fix Permissions for web-server not accessible directories", "$preCmd && chmod -R u+rwx,go-rwx scripts/ updates/ data_files/ fabric_data/ development_tools/ symfony-patch/ resource_files/ README LICENSE asterisell.php");

    if ($isAdmin) {
        myExecute("Fix Permissions for web-server only-admin readable directories", "$preCmd && chmod -R g+rx,g-w,u+rwx,o-rwx resource_files/ ");
    }
}


/**
 * In case of admin, filter only the conversion strings starting with "__".
 * For customers copy the file.
 * @param bool $isAdmin
 * @param string $inFileName
 * @param string $outFileName
 */
function convert_i18n_messages($isAdmin, $inFileName, $outFileName)
{

    if ($isAdmin) {

        $fp = fopen($inFileName, "r") or die("cannot open file $inFileName");
        $str = fread($fp, filesize($inFileName));
        $xml = new DOMDocument();
        $xml->formatOutput = true;
        $xml->preserveWhiteSpace = false;
        $xml->loadXML($str) or die("Error parsing $inFileName");

        $root = $xml->documentElement;

        $toRemove = array();
        $children = $root->firstChild->getElementsByTagName('source');
        foreach ($children as $child) {
            /**
             * @var DOMElement $child
             */

            $value = $child->textContent;

            if ($value[0] == '_' && $value[1] == '_') {
                // Node is valid

            } else {
                // Remove node in a later step otherwise iteration is broken...
                $toRemove[] = $child->parentNode;
            }
        }

        foreach ($toRemove as $child) {
            /**
             * @var DOMElement $child
             */
            $root->firstChild->firstChild->removeChild($child);

        }

        $xml->save($outFileName) or die("cannot write file $outFileName");
        fclose($fp);

    } else {
        myExecute('i18n support', 'cp -f ' . $inFileName . ' ' . $outFileName);
    }
}

/**
 * Signal as already imported some CSV file.
 * @param string $csvFileName
 * @return null if it is all ok, the error message otherwise
 */
function theseRemoteFilesAreAlreadyProcessed($csvFileName)
{
    $conn = Propel::getConnection();
    $handle = fopen($csvFileName, 'r');
    if ($handle === FALSE) {
        return "$csvFileName does not exists";
    }
    $nrOfCol = 2;
    $ln = 0;
    while (($data = fgetcsv($handle, 64000, ',', "\"", "\"")) !== FALSE) {
        $ln++;
        $nr = count($data);
        if ($nr != $nrOfCol) {
            return "at line $ln of $csvFileName there are $nr columns instead of expected $nrOfCol (" . print_r($data, TRUE) . ")";
        } else {
            $providerName = $data[0];
            $remoteFileName = $data[1];
            $stmt = $conn->prepare('SELECT id FROM ar_cdr_provider WHERE internal_name = ?');
            $stmt->execute(array($providerName));
            $providerId = null;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $providerId = $rs[0];
            }
            $stmt->closeCursor();

            if (is_null($providerId)) {
                return "unknown provider $providerName at line $ln of $csvFileName";
            }

            $now = fromUnixTimestampToMySQLTimestamp(strtotime('+0 days'));
            $stmt = $conn->prepare('INSERT INTO ar_remote_file SET ar_cdr_provider_id = ?, name = ?, receiving_date = ?');
            $stmt->execute(array($providerId, $remoteFileName, $now));
            $stmt->closeCursor();

            echo "\n   Virtually imported from $providerName : $remoteFileName";
        }
    }

    return null;
}

/**
 * @param bool $isInteractive
 * @param bool $changePermissions
 * @param bool $writeToDatabase
 * @return void
 */
function makeActivate($isInteractive = true, $changePermissions = false, $writeToDatabase = true)
{
    global $input_line;

    makeDerivedFiles();

    if ($writeToDatabase) {
        $job = CustomCDRServices::getInstance()->getRateFormatsJob();
        $job->process();
    }

    $user = getInstanceConfigValue('web_server_user');

    makeActivateDirs($user, true);

    if ($writeToDatabase) {
        echo "\nWrite settings in table ar_global_permissions";
        FieldsToShow::exportToGlobalSettingsTable();
    }

    //
    // Manage the user instance
    //

    $toExclude = array(
        '/cache/'
    , '/log/'
    , '/asterisell.php'
    , '/config/databases.yml'
    , '/data_files/'
    , '/rating_tools/'
    , '/doc/'
    , '/fabric_data/'
    , '/resource_files/'
    , '/scripts/'
    , '/symfony-patch/'
    , '/updates/');

    $toExclude_rsync = '';
    foreach ($toExclude as $d) {
        $toExclude_rsync .= " --exclude='$d' ";
    }

    myExecute("Update user instance", "rsync -ar --delete " . getAsterisellCompleteAdminDirectory() . "/ " . getAsterisellCompleteUserDirectory() . " " . $toExclude_rsync);
    // NOTE: I'm using the final "/" for not copying the admin directory inside source

    // Manage translation files, producing a version for admin and another for customer.
    $destDir = '/apps/asterisell/i18n';
    myExecute("i18n support", "mkdir " . getAsterisellCompleteUserDirectory() . $destDir);
    myExecute("i18n support", "mkdir " . getAsterisellCompleteAdminDirectory() . $destDir);

    $dir = getAsterisellCompleteAdminDirectory() . '/scripts/installation/i18n';
    $dh = opendir($dir);
    while (false !== ($filename = readdir($dh))) {
        if (isSuffixOf('.xml', $filename)) {
            $inFileName = $dir . '/' . $filename;
            $outFileName1 = getAsterisellCompleteAdminDirectory() . $destDir . '/' . $filename;
            $outFileName2 = getAsterisellCompleteUserDirectory() . $destDir . '/' . $filename;

            convert_i18n_messages(true, $inFileName, $outFileName1);
            convert_i18n_messages(false, $inFileName, $outFileName2);
        }
    }
    closedir($dh);

    myExecute("Publish manual", "ln -s " . getAsterisellCompleteAdminDirectory() . "/doc/manual/out " . getAsterisellCompleteAdminDirectory() . "/web/manual");
    // NOTE: I'm using the final "/" for not copying the admin directory inside source

    makeActivateDirs($user, false);
}

/**
 * Do not require a database connection, and derive only files.
 */
function makeDerivedFiles()
{
    myExecute("Create default directories", "mkdir -p cache ; mkdir -p log ; mkdir -p web/generated_graphs ; mkdir -p data_files/rsync_with_remote_servers ; mkdir -p data_files/messages/archive ; mkdir -p data_files/messages/backup ; mkdir -p data_files/messages/input ; mkdir -p data_files/messages/params ; mkdir -p data_files/messages/tmp ; mkdir -p updates ;  mkdir -p web/uploads/assets");

    $buildVersion = '';

    CustomCDRServices::getInstance()->calledFromMakeActivate();

    executeSymfonyCC();
    myExecute("Regenerate modules depending from the new cache values.", "cd module_templates && php generate.php");

    echo "\n";
}

/**
 * Clear symfony cache.
 *
 * @param bool $isAdmin
 * @return void
 */
function executeSymfonyCC($isAdmin = true)
{
    // NOTE: I'm executing 'cc' instead of 'cache:clear' because it seems more complete

    if ($isAdmin) {
        $preCmd = 'cd ' . getAsterisellCompleteAdminDirectory() . ' && ';
    } else {
        $preCmd = 'cd ' . getAsterisellCompleteUserDirectory() . ' && ';
    }
    myExecute("Clear symfony cache, in order to enable new settings.", "$preCmd ./symfony cc");
}

function deleteLockTableExceptJobProcessor()
{
    $conn = Propel::getConnection();
    $stmt = $conn->prepare("DELETE FROM ar_lock WHERE NOT name = ?");
    $stmt->execute(array(JobQueueProcessor::MUTEX_FILE_NAME));
}

///////////////////////
// SYSTEM MANAGEMENT //
///////////////////////

/**
 * Execute the JobProcessorQueue.
 *
 * Delete also LOCK files.
 * This is needed because during installation these files
 * have rights different from the lock files created
 * from the web server during normal/production execution.
 *
 * @param bool $silent true for not showing messages
 * @param bool $isDebugMode
 * @return void
 */
function runJobProcessorQueue($silent, $isDebugMode)
{
    $webDir = getAsterisellCompleteAdminDirectory() . DIRECTORY_SEPARATOR . 'web';

    if (!$silent) {
        echo "\nStarted Job Processor.";
    }

    // Execute the jobs immediately.
    FixedJobProcessor::setWaitForScheduledRerate(false);

    $processor = new JobQueueProcessor();
    $r = $processor->process(!$silent, $isDebugMode);

    if (!$silent) {
        if (is_null($r)) {
            echo "\nWARNING: Jobs were not processed, because lock can not be acquired.";
        } else {
            echo "\nJobs were executed.";
        }
    }
}

/**
 * Wait some time, for free cron job, otherwise exit with error code.
 *
 * @param bool $silent
 * @param bool $waitForLock true for waiting some time, until the lock is released
 * @return JobQueueProcessor the processor with the lock.
 */
function waitCronJob($silent = false, $waitForLock = true)
{

    $waitSeconds = 5;

    $processor = new JobQueueProcessor();

    $continue = TRUE;

    if ($waitForLock) {
        $retryCount = 20;
    } else {
        $retryCount = 1;
    }
    while ($continue) {
        if ($retryCount <= 0) {
            if (!$silent) {
                echo "\nBAD: there are still running Jobs. The process can not continue.";
                echo "\nYou could try with command `php asterisell.php app unlock-halted-jobs`\n";
            }
            exit(1);
        }
        $retryCount--;

        $r = $processor->lock();
        if ($r == TRUE) {
            if (!$silent) {
                echo "\nOk: there are no running Jobs, and other Jobs will be locked.\n";
            }
            $continue = FALSE;

        } else {
            if (!$silent) {
                echo "\nWaiting: there are running Jobs. I will sleep for $waitSeconds seconds, and try other $retryCount times.\n";
            }
            sleep($waitSeconds);
        }
    }

    return $processor;
}

/**
 * NOTE: it is not mandatory
 *
 * @param JobQueueProcessor $processor
 * @return void
 */
function unlockCronJob(JobQueueProcessor $processor)
{
    $processor->unlock();
}

/**
 * @return void
 */
function showMaintananceMode()
{

    if (AsterisellUser::isCronLockedForMaintanance()) {
        echo "\nWARNING:";
        echo "\n  Up to date all cron jobs are locked, because the application is in maintanance mode.";
        echo "\n  You can enable it again with command `php asterisell.php cron enable`";
        echo "\n";
    }

    if (AsterisellUser::isAppLockedForMaintanance()) {
        echo "\nWARNING:";
        echo "\n  Up to date Asterisell Web application can be accessed only from administrators, because it is in maintanance mode.";
        echo "\n  You can enable Asterisell again with command `php asterisell.php app enable`";
        echo "\n";
    }
}

///////////////
// UTILITIES //
///////////////

/**
 * @param string $srcDir
 * @param string $destDir
 * @param string $readmeFile
 */
function moveCSVFiles($srcDir, $destDir, $readmeFile)
{
    if (file_exists($destDir)) {
        if (is_dir($destDir)) {
            if (is_writable($destDir)) {
                if ($handle = opendir($srcDir)) {
                    while (false !== ($file = readdir($handle))) {
                        if (is_file($srcDir . '/' . $file) && $file !== $readmeFile) {
                            $s = rename($srcDir . '/' . $file, $destDir . '/' . $file);
                            if ($s == FALSE) {
                                echo "\n!!!!!!!! error moving $file";
                            }
                        }
                    }
                    closedir($handle);
                } else {
                    echo "$srcDir could not be opened.\n";
                }
            } else {
                echo "\n$destDir is not writable!\n";
            }
        } else {
            echo "\n$destDir is not a directory!\n";
        }
    } else {
        echo "\n$destDir does not exist\n";
    }
}

//////////////////////
// MAIN ENTRY POINT //
//////////////////////

/**
 * @param int $argc
 * @param array $argv
 * @return int exit code
 */
function main($argc, $argv)
{
    global $debugMode;

    $exitCode = 0;

    $mainCommand = '';
    $subCommand = '';
    $option1 = '';
    $option2 = '';
    $option3 = '';

    $lock = null;

    if ($argc > 1) {
        $mainCommand = trim($argv[1]);
    }

    if ($argc > 2) {
        $subCommand = trim($argv[2]);
    }

    if ($argc > 3) {
        $option1 = trim($argv[3]);
    }

    if ($argc > 4) {
        $option2 = trim($argv[4]);
    }

    if ($argc > 5) {
        $option3 = trim($argv[5]);
    }

    // NOTE: these commands do not test for database connections
    $isThereDatabaseConnection = false;
    if ($mainCommand === "install") {
        $dbRootUser = null;
        $dbRootPassword = null;
        $ask = false;
        if (!isEmptyOrNull($subCommand)) {
            $dbRootUser = $subCommand;
        } else {
            $ask = true;
        }

        if (!isEmptyOrNull($option1)) {
            $dbRootPassword = $option1;
        } else {
            $ask = true;
        }
        if ($ask) {
          explicitConfirmForDeletion();
        }
        manageCommand_install(false, $dbRootUser, $dbRootPassword);
    } else if ($mainCommand == 'install-views-and-procedures') {
        $job = new InitWithDefaultMySQLStoredProcedures();
        $job->process();
        makeActivate(true, true, true);
    } else if ($mainCommand === "install-demo") {
        $dbRootUser = null;
        $dbRootPassword = null;
        $ask = false;
        if (!isEmptyOrNull($subCommand)) {
            $dbRootUser = $subCommand;
        } else {
            $ask = true;
        }

        if (!isEmptyOrNull($option1)) {
            $dbRootPassword = $option1;
        } else {
            $ask = true;
        }

        if ($ask) {
          explicitConfirmForDeletion();
        }

        list($database, $user, $password) = getDatabaseNameUserAndPassword();

        // Test if the passwords are correct, as security measure, before deleting all data
        $retVal = 0;
        $cmd = 'mysql -u' . $dbRootUser . ' -p' . $dbRootPassword . ' ' . $database . ' -e "SELECT 0 FROM ar_cdr;"';
        system($cmd, $retVal);
        if ($retVal != 0) {
            echo "\nPassword not valid.";
            exit(1);
        }

        require_once('scripts/installation/InstallDemoData.php');
        $job = new InstallDemoData();
        $job->setCdrsToCreate(30000);
        $job->process();

    } else if ($mainCommand == "dev") {
        $exitCode = manageCommand_dev($subCommand, $option1, $option2, $option3);
    } else if ($mainCommand === "make-derived-files") {
        makeDerivedFiles();
    } else if ($mainCommand === "silently-activate") {
        makeActivate(false, true, false);
    } else if ($mainCommand === 'manage') {
        manageCommand_manage($subCommand);
    } else if ($mainCommand === "help" || $mainCommand == "") {
        displayUsage();
        return (1);
    } else {
        // test database connection, because all other commands require a safe connection

        $connectionError = "\nERROR: MySQL database user have no alter table privileges, or read privileges.\nCheck your database configurations inside `config/databases.yml` file.\nProbably Asterisell is not installed. In this case the command to do is `php asterisell.php install`, but it is better following installation instructions of the manual.\n";

        if (!isSafeReadConnection()) {
            // signal that the connection is not ok
            echo $connectionError;
            exit(1);
        }

        // HACK: force update of problem-table, in case of an upgrade from an old version, with a complete different
        // format for errors. I leave the code as reference, in case it can be useful in future.
        // maybeUpdateProblemTables();

        $isThereDatabaseConnection = true;
        if ($mainCommand === "cron") {
            if ($subCommand === "enable") {
                AsterisellUser::unlockCronForMaintanance();
            } else if ($subCommand === "disable") {
                AsterisellUser::lockCronForMaintanance();
                echo "\nIMPORTANT:\n    if you want change the application database, or doing some upgrade use \"disable-for-upgrade\" option instead, because this option is not checking there are no running jobs.\n";
            } else if ($subCommand === "disable-for-upgrade") {
                // Wait there are no running jobs, then lock only for system maintanance, and release the lock for running jobs.
                // In this way admin can run jobs, but not the normal application scheduler.
                // At the same time the application exit without errors, only if a valid lock was obtained.
                $job = waitCronJob(false, true);
                AsterisellUser::lockCronForMaintanance();
                $job->unlock();
            } else if ($subCommand === "force-execution-of-all-jobs") {
                deleteLockTableExceptJobProcessor();
                echo "\nAll scheduled jobs will be executed at next job execution pass.";
            } else {
                displayUsage();
                $exitCode = 1;
            }
        } else if ($mainCommand === "run") {
            $exitCode = manageCommand_run($subCommand);
        } else {

            // all these jobs require a lock into the database

            $lock = waitCronJob();
            try {
                if ($mainCommand === "app") {
                    $exitCode = manageCommand_app($subCommand);

                } else if ($mainCommand === "activate") {
                    if ($subCommand === 'users') {
                        $dbRootUser = null;
                        $dbRootPassword = null;
                        if (!isEmptyOrNull($option1)) {
                            $dbRootUser = $option1;
                        }
                        if (!isEmptyOrNull($option2)) {
                            $dbRootPassword = $option2;
                        }
                        makeInstallCreateUsers($dbRootUser, $dbRootPassword);
                    }
                    makeActivate();
                } else if ($mainCommand === "data") {
                    $exitCode = manageCommand_data($subCommand, $option1, $option2, $option3);
                } else if ($mainCommand === "debug") {
                    $exitCode = manageCommand_debug($lock, $subCommand, $option1, $option2, $option3);
                } else {
                    displayUsage();
                }

                unlockCronJob($lock);
            } catch (Exception $e) {
                unlockCronJob($lock);
                throw($e);
            }
        }
    }

    if ($isThereDatabaseConnection) {
        ArProblemException::commitLogTransaction();
    }

    echo "\n";
    showMaintananceMode();

    if ($debugMode) {
        echo "\nWARNING: !!! DEBUG MODE activated in the initial part of `asterisell.php` management script.\nThis consumes more resources.\n";
    }

    return $exitCode;
}

/**
 * Execute jobs, acquiring lock.
 *
 * @param string $subCommand
 * @return int exitCode
 */
function manageCommand_run($subCommand)
{

    if ($subCommand === "jobs") {
        // NOTE: execute these jobs, also if in maintainance, because they are run from administrator
        // NOTE: it will be blocked if there is another job already waiting
        runJobProcessorQueue(false, false);
    } else if ($subCommand === "scheduled-jobs") {
        if (!AsterisellUser::isCronLockedForMaintanance()) {
            runJobProcessorQueue(true, false);
        }
    } else if ($subCommand === "upgrade-jobs" || $subCommand === 'db-upgrade-jobs') {
        if ($subCommand === "upgrade-jobs") {
            $isDBUpgrade = false;
        } else {
            $isDBUpgrade = true;
        }

        if (AsterisellUser::isCronLockedForMaintanance()) {
            // Make sure there are no running jobs
            $job = waitCronJob(true, true);
            $job->unlock();

            $isOk = JobQueueProcessor::applyNewUpgradingJobs(true, $isDBUpgrade);
            $unitInfo = OrganizationUnitInfo::getInstance();
            // NOTE: doing so organization-info caching is written on the DB,
            // and it can be used immediately from customers connections.
            if ($isOk === FALSE) {
                echo "\nError during upgrade phase.\n";
                exit(1);
            } else {
                echo "\nUpgrade jobs were applied with success.\n";
            }
        } else {
            echo "\nThis command can be executed only if the application is locked for maintenance.\n";
            exit(1);
        }

    } else {
        displayUsage();
        return (1);
    }

    return (0);
}

function manageCommand_install($acquireLockOnJobProcessor = true, $dbRootUser = null, $dbRootPassword = null)
{
    $isInteractive = true;

    makeInstallCreateDatabase($dbRootUser, $dbRootPassword);
    executeSymfonyCC();

    CustomCDRServices::getInstance()->calledFromInitialInstallation();

    if ($acquireLockOnJobProcessor) {
        $lock = waitCronJob();
    } else {
        $lock = null;
    }

    // this in the first installation of the database, so all upgrades are already applied, and mark them according
    // this simplify other pass of uprade.
    JobQueueProcessor::considerUpgradingJobsAsAlreadyAppliedWithoutExecutingThem($isInteractive);
    makeActivate($isInteractive);
    JobQueueProcessor::applyInitialConfigurationJobsToTheDatabase(true);

    if ($acquireLockOnJobProcessor) {
        unlockCronJob($lock);
    }
}

function manageCommand_manage($subCommand)
{
    list($database, $user, $password) = getDatabaseNameUserAndPassword();
    if ($subCommand === 'get-database-name') {
        echo $database;
    } elseif ($subCommand === 'get-database-user') {
        echo $user;
    } elseif ($subCommand === 'get-database-user-password') {
        echo $password;
    }
}

/**
 * @param string $subCommand
 * @param string $option1
 * @param string $option2
 * @param string $option3
 * @return int exit code
 */
function manageCommand_data($subCommand, $option1, $option2 = '', $option3 = '')
{

    $password = $option1;

    if ($subCommand === "admin") {
        addRootUser($password);
    } else if ($subCommand == "unbilled") {
        if (isEmptyOrNull($option1)) {
            displayUsage();
            return(1);
        } else {
            $d = $option1;
        }

        if (!isEmptyOrNull($option2)) {
            $d .= ' ' . $option2;
        }

        $ud = fromMySQLTimestampToUnixTimestamp($d);

        $p = ArParamsPeer::getDefaultParams();
        $p->setOfficialCalldate($ud);
        $p->save();
    } else if ($subCommand === "merge-telephone-prefixes") {
        $job = new LoadWorldTelephonePrefixesFromCSVFile();
        $job->loadAllPrefixes(true);
    } else if ($subCommand === "backup") {
        makeDatabaseBackup(true, false);
    } else if ($subCommand === "config-backup") {
        makeDatabaseBackup(true, true);
    } else if ($subCommand == "restore") {
        $status = makeDatabaseRestore(true);
        if ($status === TRUE) {
            echo "\nData restored.\n";
        } else if ($status === FALSE) {
            echo "\nData not restored.\n";
        } else {
            echo "\nError during restore of data. $status\n";
            return (1);
        }
    } else if ($subCommand == "update-cached-cdrs") {
        echo "\n\nUpdate ar_cached_cdrs and ar_cached_errors tables.\n\n";
        if (!RateEngineService::executeUpdateAllCachedCDRS()) {
            echo "Error during updating of ar_cached_cdrs and ar_cached_errors table\n";
            return (1);
        }
    } else if ($subCommand == "import-organizations") {
        $fileName = $option1;
        if (!file_exists($fileName)) {
            echo "File \"$fileName\" does not exists.";
            return (1);
        }

        $isNew = false;
        if (!isEmptyOrNull($option2) && strcmp($option2, "new") == 0) {
            $isNew = true;
        }

        $yamlContent = file_get_contents($fileName);
        $job = new ChangeOrganizationInfo();
        try {
            $job->processYAMLContent($yamlContent, $isNew);
            echo "\nFile imported correctly.\n";
        } catch (ArProblemException $e) {
            echo "\nError importing the file: " . ArProblemException::getLastErrorDescription();
            return (1);
        }
    } else if ($subCommand == "export-organizations") {
        $yamlJob = new ChangeOrganizationInfo();
        $yaml = $yamlJob->getYAMLContent(null, null, null);
        if (isEmptyOrNull($option1)) {
          echo $yaml;
          echo "\n";
        } else {
            file_put_contents($option1, $yaml);
            echo "\nWritten to file $option1";
        }
    } else if ($subCommand == "complete-reseller-export-code") {
        if (!isEmptyOrNull($option1)) {
            $resellerCode = $option1;
            $resellerId = ExportCDRSToReseller::fromResellerCodeToUnitId($resellerCode);
            if (is_null($resellerId)) {
                echo "\nUnknown reseller code \"$resellerCode\"";
            } else {
                completeResellerExportCodeId($resellerId);
            }
        } else {
            echo "\nNeeded a reseller code";
        }
    } else if ($subCommand == "delete-organization") {
        if (!isEmptyOrNull($option1)) {
            $id = intval($option1);
            OrganizationUnitInfo::deletePhysicallyOrganization($id);
            echo "\nDeleted organization with id $id";
        } else {
            echo "\nNeeded an id of an extension/organization";
        }
    } else if ($subCommand == "export-cdrs") {

        $cdrProviderName = $option1;
        $cdrFormatName = $option2;
        $cdrTimeFrame = $option3;

        if (isEmptyOrNull($cdrProviderName)) {

            // Show a list of supported format
            $query = 'SELECT p.internal_name, l.name, f.name
                      FROM ar_type_of_source_cdr AS t
                      INNER JOIN ar_cdr_provider AS p ON t.ar_cdr_provider_id = p.id
                      INNER JOIN ar_physical_format AS f ON t.ar_physical_format_id = f.id
                      INNER JOIN ar_logical_source AS l ON f.ar_logical_source_id = l.id
                      ORDER BY p.internal_name, l.name, f.name';

            $stm = Propel::getConnection()->prepare($query);
            $stm->execute();

            echo "\nAvailable options (cdr-provider-code cdr-format) are:\n";
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {

                echo "\n    " . $rs[0] . " " . $rs[1] . '__' . $rs[2];
            }
            $stm->closeCursor();
            echo "\n";
            echo "\nUse as time-frame \"yyyy-mm\" for exporting a month, or \"yyyy-mm-dd\" for exporting a day.\n";
        } else {

            $m = array();

            $statusYYYY = null;
            $statusMM = null;
            $statusDD = null;

            if (preg_match('/^(\d\d\d\d)-(\d\d)-(\d\d)$/i', $cdrTimeFrame, $m)) {

                $statusYYYY = intval($m[1]);
                $statusMM = intval($m[2]);
                $statusDD = intval($m[3]);

            } else if (preg_match('/^(\d\d\d\d)-(\d\d)$/i', $cdrTimeFrame, $m)) {

                $statusYYYY = intval($m[1]);
                $statusMM = intval($m[2]);
                $statusDD = null;
            } else {
                echo "\nTime frame \"$cdrTimeFrame\", has an unknown format.\n";
                return (1);
            }

            $m = array();

            if (preg_match('/^([^.]+)__([^.]+)$/i', $cdrFormatName, $m)) {

                $logicalTypeName = $m[1];
                $formatName = $m[2];

            } else {
                echo "\nFormat name \"$cdrFormatName\", has an unknown format.\n";
                return (1);
            }

            $job = new ImportDataFiles();
            $fileName = $job->exportStatusFile('exported', False, $cdrProviderName, $cdrProviderName, $logicalTypeName, $formatName, $statusYYYY, $statusMM, $statusDD);

            if (is_null($fileName)) {
                echo "\nError processing request\n";
                return (2);
            }

            $format1 = ArLogicalSourcePeer::retrieveByName($logicalTypeName);
            $format2 = ArPhysicalFormatPeer::retrieveByName($format1->getId(), $formatName);

            echo "\nInfo about $logicalTypeName:\n" . $format1->getDescription();
            echo "\n\n";
            echo "Info about specific $logicalTypeName" . "__" . $formatName . " version:\n" . $format2->getDescription();
            echo "\n\n";
            echo "Produced file\n\n   $fileName\n\nYou can modify the file content, and put it into input directory\n\n   " . ImportDataFiles::getAbsoluteInputDirectory() . "\n\nfor loading a new version of the data in the specified time frame.\nImportant: do not change last part of file name because it is used for recognizing the format and time frame.\nImportant: you can maintain the original file, in order to restore previous info in case of errors.\n";

            return (0);
        }
    } else {
        displayUsage();
        return (1);
    }

    return (0);
}

/**
 * @param string $subCommand
 * @return int exitCode
 */
function manageCommand_app($subCommand)
{
    if ($subCommand === "enable") {
        AsterisellUser::unlockAppForMaintanance();
    } else if ($subCommand === "disable") {
        AsterisellUser::lockAppForMaintanance();
    } else if ($subCommand === 'produce-code-template') {
        echo "\n\n// Copy and paste this function inside\n\n// > apps/asterisell/lib/jobs/reports/BaseBillingReportCalcStore.php\n\n";
        echo createCodeFor_getValues();
        echo "\n";
    } else {
        displayUsage();
        return (1);
    }

    return (0);
}

/**
 * @param JobQueueProcessor $lock an active lock
 * @param string $subCommand
 * @param string $option1
 * @param string $option2
 * @param string $option3
 * @return int exit code
 */
function manageCommand_debug(JobQueueProcessor $lock, $subCommand, $option1, $option2, $option3 = '')
{
    if ($subCommand === "jobs") {
        $lock->unlock();

        // NOTE: debug mode is also tested at the init of this script and used for setting DEV mode.
        runJobProcessorQueue(false, true);
    } else if ($subCommand == 'regression-test') {
        $dbRootUser = null;
        $dbRootPassword = null;
        $ask = false;
        if (!isEmptyOrNull($option1)) {
            $dbRootUser = $option1;
        } else {
            $ask = true;
        }

        if (!isEmptyOrNull($option2)) {
            $dbRootPassword = $option2;
        } else {
            $ask = true;
        }

        if ($ask) {
           echo "\nMissing root and password.\n";
           exit(1);
        }

        list($database, $user, $password) = getDatabaseNameUserAndPassword();

        require_once('scripts/installation/CommonTests.php');
        manageCommand_install(false, $dbRootUser, $dbRootPassword);
        $job = new CommonTests();
        $job->process();

        $lock->unlock();

        echo "\n\nAll tests were executely correctly\n";

    } else if ($subCommand == 'stress-rerating') {
        $lock->unlock();
        $job = new ManageRateEvent();
        $r = $job->stressRerating($option1, $option2, true);
        if ($r) {
            echo "\nThere were no errors after various rerating attempts.\n";
        } else {
            echo "\n!!! There are errors after various rerating attempts. !!!\n";
            echo "\n!!! The database is left in an inconsistent state. Rerate manually all the CDRS. !!!\n";
            return (1);
        }

    } else if ($subCommand === "rerate" || $subCommand === "rate" || $subCommand === "debug-rate") {
        $allOk = true;

        if (isEmptyOrNull($option1)) {
            FixedJobProcessor::rerateCallsFromOfficialCalldate(false);
        } else {
            $callDate1 = null;

            $callDate1 = fromMySQLTimestampToUnixTimestamp($option1);
            if (is_null($callDate1)) {
                $allOk = false;
            }

            if ($allOk) {
                FixedJobProcessor::rerateCalls($callDate1);
            }
        }

        if ($allOk) {
            if ($subCommand === 'rerate') {
                echo "\nGenerated a rerating event. CDRs will be completely rerated at next execution of job processor.\n";
            } else if ($subCommand === 'rate' || $subCommand === 'debug-rate') {
                echo "\nStart rating event\n";

                ArProblemException::beginLogTransaction();
                $job = new ManageRateEvent();
                if ($subCommand === 'debug-rate') {
                    $job->setDebugMode(true);
                }
                $log = $job->process();
                ArProblemException::commitLogTransaction();
                echo "\n" . $log . "\n";
            }
        } else {
            echo "\nERROR: call dates are not in format \"YYYY-MM-DD\" with optional \"HH:MM:SS\" part.\n";
            return (1);
        }
    } else if ($subCommand == 'reset-rerate-event') {
        FixedJobProcessor::signalAsDoneRerateCallsFromOfficialCalldate(null, true);
        FixedJobProcessor::rerateCalls(null);
    } else if ($subCommand == 'some-code-test') {
        someCodeToTest();
    } else if ($subCommand == 'test') {
        $jobClass = $option1;

        /**
         * @var FixedJobProcessor $job
         */
        $job = new $jobClass();
        $job->setDebugMode(true);
        $job->regressionTestParam = $option2;

        echo "\nStart regression test " . $jobClass . ": " . $option2;

        $job->process();
    } else if ($subCommand == 'signal-critical-problem-in-the-code') {
        ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_CRITICAL,
            ArProblemDomain::APPLICATION,
            null,
            "severe error in the PHP code",
            "The application code contains a severe PHP error, forcing the aborting of the application.",
            "Current Jobs were interrupted. If the problem persist, no new CDRs can be rated. The JobLog contains a detail of job forcing an interruption of the job chain/sequence.",
            "This is an error in the application code. Contact the assistance.",
            null);
    } else {
        displayUsage();
    }

    return (0);
}

/**
 * @param string $subCommand
 * @param string $option1
 * @param string $option2
 * @param string $option3
 * @return int exit code
 */
function manageCommand_dev($subCommand, $option1 = '', $option2 = '', $option3 = '')
{
    if ($subCommand === "remove-model") {

        $n = $option1;

        myRemoveFileWithInfo('lib/filter/' . $n . 'FormFilter.class.php');
        myRemoveFileWithInfo('lib/filter/base/Base' . $n . 'FormFilter.class.php');

        myRemoveFileWithInfo('lib/form/' . $n . 'Form.class.php');
        myRemoveFileWithInfo('lib/form/base/Base' . $n . 'Form.class.php');

        myRemoveFileWithInfo('lib/model/' . $n . '.php');
        myRemoveFileWithInfo('lib/model/' . $n . 'Peer.php');
        myRemoveFileWithInfo('lib/model/map/' . $n . 'TableMap.php');
        myRemoveFileWithInfo('lib/model/map/' . $n . 'MapBuilder.php');
        myRemoveFileWithInfo('lib/model/om/Base' . $n . '.php');
        myRemoveFileWithInfo('lib/model/om/Base' . $n . 'Peer.php');

    } else if ($subCommand === "list-jobs") {
        $job = new JobQueueProcessor();
        $job->displayJobsWithDevNotes();
    } else if ($subCommand === 'update-customizations') {
        // up to date activate is doing the same thing. In future this procedure can make more complex things.
        makeActivate(true, true, true);
    } else if ($subCommand === 'support-user') {
        $password = trim($option1);
        if (isEmptyOrNull($password)) {
            echo "\nNot valid password.\n";
            return (1);
        }
        $login = 'support';
        $user = ArUserPeer::retrieveByLogin($login);
        if (is_null($user)) {
            $user = new ArUser();
        }
        $user->setLogin($login);
        $user->setIsEnabled(true);
        $user->setIsRootAdmin(true);
        $user->setArParty(null);
        $user->setArOrganizationUnit(null);
        $user->setClearPassword($password);
        $user->save();

        echo "\nCreated user $login with password $password\n";
    } else if ($subCommand === 'reinstall_dev') {
        JobQueueProcessor::applyInitialConfigurationJobsToTheDatabase(true);
    } else if ($subCommand === 'these-remote-files-are-already-processed') {
        $r = theseRemoteFilesAreAlreadyProcessed($option1);
        if (is_null($r)) {
            return (0);
        } else {
            echo "\n\n" . $r;
            return (1);
        }
    } else {
        displayUsage();
        return (1);
    }

    return (0);
}

function myRemoveFileWithInfo($f)
{

    if (file_exists($f)) {
        $isOk = unlink($f);

        if ($isOk) {
            echo "\n   removed $f";
        } else {
            echo "\n   !!! problems removing $f";
        }
    } else {
        echo "\n   does not exists $f";
    }
}

/**
 * Produce some boring PHP code in an automatic way.
 *
 * @return string PHP code for acessing values of an array
 */
function createCodeFor_getValues()
{

    $r = <<<'PHP'
    /**
     * Sum all the values.
     *
     * NOTE: this code is "efficient", only because there are few destination types,
     * and vendors, and communication channel to sum every time.
     *
     * NOTE: this code is produced in an automatic way,
     * and the paste-bin in this class, from:
     *
     * > php asterisell.php app produce-code-template
     *
     * @param string|null $ids
     * @param int|null $destinationType null for summing all values
     * @param int|null $vendorId null for summing all values
     * @param int|null $communicationChannelId null for summing all values
     * @param int|null $geographicLocation
     * @param int|null $operatorType
     * @return array values: list(count of calls, duration, cost, income, cost savings)
     */
    public function getValues($ids, $destinationType, $vendorId, $communicationChannelId, $geographicLocation, $operatorType) {

    $totCount = 0;
    $totDuration = 0;
    $totCost = 0;
    $totIncome = 0;
    $totSavings = 0;

    if (is_null($ids)) {
      // the special identifier for the super root
      $ids = '/';
    }

    if (isset($this->values[$ids])) {

PHP;

    foreach (array(true, false) as $specificDestinationType) {
        $r .= "\n";
        if ($specificDestinationType) {
            $r .= 'if(! is_null($destinationType)) {';
        } else {
            $r .= "\n } else { ";
        }

        foreach (array(true, false) as $specificVendorId) {
            $r .= "\n";
            if ($specificVendorId) {
                $r .= 'if(! is_null($vendorId)) {';
            } else {
                $r .= "\n } else { ";
            }

            foreach (array(true, false) as $specificCommunicationChannelId) {
                $r .= "\n";
                if ($specificCommunicationChannelId) {
                    $r .= 'if(! is_null($communicationChannelId)) {';
                } else {
                    $r .= "\n } else { ";
                }


                foreach (array(true, false) as $specificGeographicLocation) {
                    $r .= "\n";
                    if ($specificGeographicLocation) {
                        $r .= 'if(! is_null($geographicLocation)) {';
                    } else {
                        $r .= "\n } else { ";
                    }

                    foreach (array(true, false) as $specificOperatorType) {
                        $r .= "\n";
                        if ($specificOperatorType) {
                            $r .= 'if(! is_null($operatorType)) {';
                        } else {
                            $r .= "\n } else { ";
                        }

                        $r .= "\n\n" . creatCodeFor_getValues1($specificDestinationType, $specificVendorId, $specificCommunicationChannelId, $specificGeographicLocation, $specificOperatorType);

                        if (!$specificOperatorType) {
                            $r .= "\n}";
                        }
                    }

                    if (!$specificGeographicLocation) {
                        $r .= "\n}";
                    }
                }

                if (!$specificCommunicationChannelId) {
                    $r .= "\n}";
                }
            }
            if (!$specificVendorId) {
                $r .= "\n}";
            }
        }
        if (!$specificDestinationType) {
            $r .= "\n}";
        }
    }

    $r .= '
    } else {
      return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
    }
    ';

    $r .= "\n}";
    return $r;
}

/**
 * @param bool $specificDestinationType
 * @param bool $specificVendorId
 * @param bool $specificCommunicationChannelId
 * @param bool $specificGeographicLocation
 * @param bool $specificOperatorType
 * @return string
 */
function creatCodeFor_getValues1($specificDestinationType, $specificVendorId, $specificCommunicationChannelId, $specificGeographicLocation, $specificOperatorType)
{

    $params = array(
        array('$destinationType', $specificDestinationType),
        array('$vendorId', $specificVendorId),
        array('$communicationChannelId', $specificCommunicationChannelId),
        array('$geographicLocation', $specificGeographicLocation),
        array('$operatorType', $specificOperatorType),
    );

    // start looping on $arr0
    $r = '$arr0 = $this->values[$ids];';
    $c = 0;
    $currArr = '';
    foreach ($params as $param) {
        list($indexName, $useIndex) = $param;

        $prevArr = '$arr' . $c;

        $c++;
        $currArr = '$arr' . $c;

        $r .= "\n";

        if ($useIndex) {
            $r .= "if (isset($prevArr" . "[$indexName])) {\n$currArr = $prevArr" . "[$indexName];";
        } else {
            $r .= "foreach($prevArr as $currArr) {";
        }
    }

    $r .= "\n" . 'list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = ' . $currArr . ';';
    $r .= <<<'PHP'

    $totCount += $totCount1;
    $totDuration += $totDuration1;
    $totCost += $totCost1;
    $totIncome += $totIncome1;
    $totSavings += $totSavings1;

PHP;

    foreach ($params as $param) {
        $r .= "\n}";
    }

    $r .= "\n" . 'return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);';

    return $r;
}

/**
 * Complete codes to export to resellers.
 *
 * @param int $rootId
 */
function completeResellerExportCodeId($rootId)
{
    $info = OrganizationUnitInfo::getInstance();

    /**
     * @var ArOrganizationUnit $org
     */
    $org = ArOrganizationUnitPeer::retrieveByPK($rootId);
    assert(!is_null($org));

    if (isEmptyOrNull($org->getExportCode())) {
        $info = OrganizationUnitInfo::getInstance();
        $data = $info->getDataInfo($rootId, time());

        $codesS = $data[OrganizationUnitInfo::DATA_EXTENSION_CODES];
        if (!isEmptyOrNull($codesS)) {
            $codes = explode(',', $codesS);
            $code = $codes[0];
        } else {
            // in case of organization (no extensions) use a symbolic name.
            // It is used in case of services and bundle rates.
            $code = 'unit-' . $rootId;
        }

        echo "\n  Set export code of " . $info->getFullNameAtDate($rootId, time(), false, false, null, false, false) . " to " . $code;

        $org->setExportCode($code);
        $org->save();
    }

    $allIds = $info->getAllDirectChildren($rootId);

    foreach ($allIds as $v) {
        list($id, $fromDate) = $v;
        completeResellerExportCodeId($id);
    }
}

/**
 * Some code to test, during development phase.
 */
function someCodeToTest()
{
    $v = array('parent_id' => NULL);
    if (array_key_exists('parent_id', $v)) {
        echo 'ok';
    } else {
        echo 'no';
    }
}

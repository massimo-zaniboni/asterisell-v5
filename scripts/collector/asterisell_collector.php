<?php

/**
 * Copyright (C) 2013 Massimo Zaniboni,
 * <massimo.zaniboni@asterisell.com>,
 * Italian Fiscal Code: ZNBMSM74L01F257Z.
 * All Rights Reserved.
 *
 * This utility can be freely used, modified, distributed.
 *
 * This SOFTWARE PRODUCT is distributed in the hope that it will be
 * useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

/*
 * This utility sends data files to remote Asterisell server.
 *
 * Usually it is invoked from the cron daemon.
 *
 * It works in a conservative way: it tries to send all the information, and it stops whenever there is a problem
 * in the processing queue. In this way local administrators are forced to solve all the problems.
 */

///////////////////
// GLOBAL PARAMS //
///////////////////

/**
 * Special file where reading user input.
 */
$input_line = fopen('php://stdin', 'r');

/**
 * @var string
 */
$ping_file_name = 'ping.0000-00-00.ping__v1';

/**
 * The local directory where put the generated files, before installing them.
 */
$configureDirectoryName = 'asterisell_collector_configs';

/**
 * @var string the settings file used during this execution of the script.
 */
$usedGlobalSettingsCompleteFileName = '';

/**
 * @var array execution settings
 */
$settings = array();

$temp_file_name = 'asterisell_temp_file.tmp';

$stat_file_about_communication_problems = 'asterisell_collect_remote_communication_problems.dat';

/////////////////
// ENTRY POINT //
/////////////////

error_reporting(E_ALL);
ini_set('display_errors', 'on');
set_error_handler("myErrorHandler");

main($argc, $argv);

function displayHelp()
{
    $name = basename(__FILE__);

    echo "USAGE: php $name help\n";
    echo "or     php $name configure OS-VERSION VOIP-SERVER-VERSION\n";
    echo "or     php $name generate\n";
    echo "or     php $name install\n";
    echo "or     php $name run file\n";
    echo "\n";
    echo "OS-VERSION accepted values are: generic-linux, redhat5, redhat6, centos5, centos6\n";
    echo "\n";
    echo "VOIP-SERVER-VERSION accepted values are: generic, asterisk\n";
    echo "\n";
    echo "\nIf this is the first time you are using this script, execute \"configure\", then follow the displayed instructions.\n";
}

function main($argc, $argv)
{
    try {
        if ($argc == 1) {
            displayHelp();
            exit(0);
        } else if ($argc == 2) {
            if ($argv[1] == 'help') {
                displayHelp();
                exit(0);
            } else if ($argv[1] == 'generate') {
                generateConfigurations();
                exit(0);
            } else if ($argv[1] == 'install') {
                installConfigurations();
                exit(0);
            }
        } else if ($argc == 3) {
            if ($argv[1] == 'run') {
                execute($argv[2]);
                exit(0);
            }
        } else if ($argc == 4) {
            if ($argv[1] == 'configure') {
                generateConfigure($argv[2], $argv[3]);
                exit(0);
            }
        }

        echo "\nUnknown params.\n\n";
        displayHelp();
        exit(2);
    } catch (Exception $e) {
        signalError($e->getMessage(), $e->getTraceAsString());
        exit(2);
    }
}

/////////////////////
// MANAGE SETTINGS //
/////////////////////

/**
 * @param string $file_settings
 * @throws Exception
 */
function load_settings($file_settings)
{
    global $usedGlobalSettingsCompleteFileName;
    global $settings;

    $usedGlobalSettingsCompleteFileName = $file_settings;

    $settings = parse_ini_file($file_settings, true);
    if ($settings == FALSE) {
        throw(new Exception("Unable to load settings in file \"$file_settings\""));
    }
}

/**
 * @param string $section
 * @param string $param
 * @return mixed
 * @throws Exception
 */
function get_settings($section, $param)
{
    global $usedGlobalSettingsCompleteFileName;
    global $settings;

    if (array_key_exists($section, $settings)) {
        if (array_key_exists($param, $settings[$section])) {
            return $settings[$section][$param];
        }
    }

    throw(new Exception("Unknown configuration parameter \"$param\", in section \"$section\", of configuration file \"$usedGlobalSettingsCompleteFileName\""));
}

////////////////////////////////////////////
// SEND LOG FILE CONTENT TO REMOTE SERVER //
////////////////////////////////////////////

/**
 * @param string $fileWithSettings
 */
function execute($fileWithSettings)
{
    try {
        load_settings($fileWithSettings);

        $max_seconds_to_wait_before_sending = get_settings('scheduler', 'max_random_seconds_before_sending');

        openLockFile();

        // call this first because log-rotate can overwrite them
        queue_log_files_in_tmp_directory();

        // note: this wait allows some multithread process to write additional info to the moved files
        wait_random_seconds(intval($max_seconds_to_wait_before_sending));

        queue_tmp_files_in_sending_directory();

        send_all_queued_files();

        // send the ping message only at the end of the processing
        create_ping_message();
        send_all_queued_files();

        closeLockFile();

    } catch (Exception $e) {
        signalErrorAndExit($e->getMessage(), $e->getTraceAsString());
        exit(1);
    }

    exit(0);
}


/**
 * Send a file to the remote server, using WebDAV protocol.
 *
 * @param string $inputFilename
 * @param string $outFileName
 * @return Void
 * @throws Exception
 */
function webdav_put($inputFilename, $outFileName)
{

    $webdav_server = get_settings('remote', 'messages_server');
    $webdav_password = get_settings('remote', 'password');
    $instance_code = get_settings('remote', 'instance_code');

    $fileSize = filesize($inputFilename);

    $fh = fopen($inputFilename, 'rb');
    if ($fh == FALSE) {
        throw(new Exception("Unable to open file \"$inputFilename\""));
    }

    $remoteUrl = "$webdav_server/$instance_code/messages/input";

    $outFileNameComplete = $remoteUrl . $outFileName;

    $ch = curl_init($remoteUrl);
    if ($ch == FALSE) {
        throw(new Exception("Unable to access remote address \"$remoteUrl\""));
    }

    curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_setopt($ch, CURLOPT_USERPWD, "$instance_code:$webdav_password");
    curl_setopt($ch, CURLOPT_URL, $outFileNameComplete);

    curl_setopt($ch, CURLOPT_NOPROGRESS, true);
    curl_setopt($ch, CURLOPT_FAILONERROR, true);

    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, !get_settings('remote', 'accept_self_signed_certificate_from_webdav_server'));

    curl_setopt($ch, CURLOPT_PUT, true);
    curl_setopt($ch, CURLOPT_INFILE, $fh);
    curl_setopt($ch, CURLOPT_INFILESIZE, $fileSize);

    curl_setopt($ch, CURLOPT_CONNECTTIMEOUT, 30);

    curl_setopt($ch, CURLOPT_BINARYTRANSFER, true);

    curl_exec($ch);

    if (!(curl_errno($ch) == 0)) {
        throw(new Exception("Unable to send \"$inputFilename\" to remote address \"$remoteUrl\". Error: " . curl_error($ch)));
    }

    $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    if ($http_code < 200 || $http_code >= 300) {
        throw(new Exception("Unable to send \"$inputFilename\" to remote address \"$remoteUrl\". HTTP-code: $http_code"));
    }

    $isOk = fclose($fh);
    if ($isOk == FALSE) {
        throw(new Exception("Unable to close correctly \"$inputFilename\"."));
    }
}

/**
 * @param string $source
 * @param string $dest
 * @return bool false in case of error
 */
function gzcompressfile($source, $dest)
{
    $mode = 'wb9';

    if ($fp_out = gzopen($dest, $mode)) {
        if ($fp_in = fopen($source, 'rb')) {

            while (!feof($fp_in)) {
                gzwrite($fp_out, fread($fp_in, 1024 * 512));
            }
            $isOk = fclose($fp_in);
            $isOk = gzclose($fp_out) && $isOk;
            $error = !$isOk;
        } else {
            $error = true;
            gzclose($fp_out);
        }
    } else {
        $error = true;
    }

    if ($error) {
        if (file_exists($dest)) {
            unlink($dest);
        }
        return false;
    } else {
        return true;
    }
}

/**
 * From the file in LOG directory
 *
 * > some-name.logical-source__physical-format
 *
 * to the file in tmp directory
 *
 * > some-name__time__unique-id.logical-source__physical-format
 *
 * @throws Exception
 */
function queue_log_files_in_tmp_directory()
{
    $sourceDir = get_settings('local', 'cdrs_log_directory');
    $tmpDir = get_settings('local', 'tmp_directory');

    $postrotateCommand = get_settings('local', 'log_postrotate_command');

    $sourceFiles = scandir($sourceDir);
    if ($sourceFiles == FALSE) {
        throw(new Exception("Unable to open \"$sourceDir\"."));
    }

    foreach ($sourceFiles as $sourceFile) {
        if ($sourceFile === '.' || $sourceFile === '..') {
            continue;
        }

        $completeSourceFile = normalizeFileNamePath($sourceDir . '/' . $sourceFile);

        if (is_file($completeSourceFile)) {

            $sourceFileParts = explode('.', $sourceFile);
            if (count($sourceFileParts) > 1) {
                $sourceFileExtension = '.' . array_pop($sourceFileParts);
                $sourceFileBase = implode('.', $sourceFileParts);
            } else {
                $sourceFileExtension = '';
                $sourceFileBase = $sourceFile;
            }

            // NOTE: files of the same type must be ordered according receiving time,
            // because in case of status files, it is important to process first the older files.
            // NOTE: the file name is guarantee to be unique
            $tmpFile = get_ordered_timeprefix_with_unique_id() . '__' . $sourceFileBase . $sourceFileExtension;

            $completeTmpFile = normalizeFileNamePath($tmpDir . '/' . $tmpFile);

            $isOk = rename($completeSourceFile, $completeTmpFile);
            if ($isOk == FALSE) {
                throw(new ErrorException("Error during processing of LOG file \"$completeSourceFile\": unable to move collected CDRs into temporary file \"$completeTmpFile\"."));
            }

            // After rename, the syslog continue adding content to the renamed log file.
            // It must be restarted in order to create a new log file.

            $returnValue = -1;
            $output = '';
            exec($postrotateCommand, $output, $returnValue);
            if ($returnValue != 0) {
                // try to restore the original file, in order to not loose information,
                // hoping that the log server is still active.
                rename($completeTmpFile, $completeSourceFile);
                throw(new ErrorException("Unable to restart the log server, using command \"$postrotateCommand\". Collection of CDRs can be interrupted."));
            }
        }
    }
}

/**
 * @return string a unique time prefix ordered by seconds and microseconds
 */
function get_ordered_timeprefix_with_unique_id() {
    $time = microtime(false);

    $p = strpos($time, ' ');
    $time1 = substr($time, 0, $p);
    $time2 = substr($time, $p + 1);

    $time1 = str_replace('.', '', $time1);

    return 't_' . $time2 . "-" . $time1 . '-' . rand(0, 5000000);
}

/**
 * From the file in TMP directory
 *
 * > some-name__time__unique-id.logical-source__physical-format
 *
 * to the file in send directory
 *
 * > some-name__time__unique-id.logical-source__physical-format.gz
 *
 */
function queue_tmp_files_in_sending_directory()
{
    global $temp_file_name;
    global $stat_file_about_communication_problems;

    $sourceDir = get_settings('local', 'tmp_directory');
    $dstDir = get_settings('local', 'send_queue_directory');

    $sourceFiles = scandir($sourceDir);
    if ($sourceFiles == FALSE) {
        throw(new Exception("Unable to open \"$sourceDir\"."));
    }

    foreach ($sourceFiles as $sourceFile) {
        if ($sourceFile === '.' || $sourceFile === '..' || $sourceFile === $stat_file_about_communication_problems) {
            continue;
        }

        $completeSourceFile = normalizeFileNamePath($sourceDir . '/' . $sourceFile);
        if (is_file($completeSourceFile)) {

            $dstFile = $sourceFile . '.gz';
            $completeDstFile = normalizeFileNamePath($dstDir . '/' . $dstFile);

            // NOTE: use a temporary file, in order to not send temporary gzipped files, in case of problems.
            $completeTmpFile = normalizeFileNamePath($dstDir . '/' . $temp_file_name);
            @unlink($completeTmpFile);

            if (gzcompressfile($completeSourceFile, $completeTmpFile)) {
                $isOk = rename($completeTmpFile, $completeDstFile);
                if ($isOk == FALSE) {
                    throw(new Exception("Unable to rename \"$completeTmpFile\" into \"$completeDstFile\"."));
                }

                $isOk = unlink($completeSourceFile);
                if ($isOk == FALSE) {
                    throw(new Exception("Unable to delete \"$completeSourceFile\". This is a critical error, because this file will be sent again, and there will be repeated CDRs sent to the Asterisell server."));
                }

            } else {
                throw(new Exception("Unable to compress \"$completeSourceFile\", into \"$completeTmpFile\"."));
            }
        }
    }
}

/**
 * Wait a random number of seconds.
 * In this way not all connections are made at the same time.
 *
 * @param $max_seconds_to_wait
 */
function wait_random_seconds($max_seconds_to_wait)
{
    $instance_code = get_settings('remote', 'instance_code');

    $seconds = crc32($instance_code) % $max_seconds_to_wait;

    sleep($seconds);
}

/**
 * Send a ping file to the server,
 * signaling that the client is still active.
 */
function create_ping_message()
{
    global $ping_file_name;
    $tmp_directory = get_settings('local', 'send_queue_directory');

    $fileName = $tmp_directory . '/' . $ping_file_name;

    $fh = fopen($fileName, 'w');
    if ($fh == FALSE) {
        throw(new Exception("Unable to create \"$fileName\". The Asterisell server will think that there are no communications with this client, because it does not receive a ping file."));
    } else {
        $isOk = fwrite($fh, time());
        fclose($fh);

        if ($isOk == FALSE) {
            throw(new Exception("Unable to create \"$fileName\". The Asterisell server will think that there are no communications with this client, because it does not receive a ping file."));
        }
    }
}

/**
 * Send the files in the queue directory, to the Asterisell server.
 *
 * @throws Exception
 */
function send_all_queued_files()
{
    $sourceDir = get_settings('local', 'send_queue_directory');

    $sourceFiles = scandir($sourceDir);
    if ($sourceFiles == FALSE) {
        throw(new Exception("Unable to open \"$sourceDir\""));
    }

    foreach ($sourceFiles as $sourceFile) {
        if ($sourceFile === '.' || $sourceFile === '..') {
            continue;
        }

        $completeSourceFile = $sourceDir . $sourceFile;

        if (is_file($completeSourceFile)) {
            $m = array();

            if (preg_match('/^(.+)\.(.+)__([^.]+)(\.gz)?$/i', $sourceFile, $m)) {
                $baseName = $m[1];
                $logicalSourceCode = $m[2];
                $formatCode = $m[3];
                if (array_key_exists(4, $m)) {
                    $gzipSuffix = $m[4];
                } else {
                    $gzipSuffix = '';
                }

                webdav_put($completeSourceFile, $baseName . $gzipSuffix, $logicalSourceCode, $formatCode);

                $isOk = unlink($completeSourceFile);
                if ($isOk == FALSE) {
                    throw(new Exception("Unable to delete \"$completeSourceFile\". This is a critical error, because this file will be sent again, and there will be repeated CDRs sent to the Asterisell server."));
                }
            } else {
                throw(new Exception("The file name \"$sourceFile\" has no name of type \"some-file-name.some-logical-format__some-format-version.gz\" (with optional \".gz\"). This file is not sent to the Asterisell server."));
            }
        }
    }
}

//////////////////////
// ERROR MANAGEMENT //
//////////////////////

/**
 * Intercept errors.
 *
 * @param $errno
 * @param $errstr
 * @param $errfile
 * @param $errline
 * @return bool
 */
function myErrorHandler($errno, $errstr, $errfile, $errline)
{
    if (!(error_reporting() & $errno)) {
        // Continue with normal code, because it is not an important error.
        return true;
    }

    $message = "Error [$errno] $errstr on line $errline in file $errfile";
    signalErrorAndExit($message);

    return false;
}

/**
 * @param string $message
 * @param string $trace
 * @return Void
 */
function signalErrorAndExit($message, $trace = '')
{
    signalError("CDRs data can not be sent to Asterisell remote server. " . $message, $trace);
    closeLockFile();
    exit(1);
}

/**
 * @param string $message
 * @param string $trace
 * @return Void
 */
function signalError($message, $trace = '')
{

    try {
        $signal_error_using_stderr = get_settings('errors_reporting', 'signal_error_using_stderr');
        $signal_errors_using_syslog = get_settings('errors_reporting', 'signal_errors_using_syslog');
    } catch (Exception $e) {
        $signal_error_using_stderr = true;
        $signal_errors_using_syslog = false;
    }

    $isSyslogOK = true;
    if ($signal_errors_using_syslog) {
        $isSyslogOK = openlog('asterisell', LOG_NDELAY, LOG_CRON);
        if ($isSyslogOK) {
            $isSyslogOK = syslog(LOG_ERR, $message . "\n" . $trace);
        }
    }

    if ($isSyslogOK == FALSE) {
        $message = "Warning: unable to write this error message to syslog.\n" . $message;
    }

    if ($signal_error_using_stderr || !$isSyslogOK) {
        $fe = fopen('php://stderr', 'w');
        fwrite($fe, $message . "\n" . $trace);
    }

}

//////////////////////////
// LOCK FILE MANAGEMENT //
//////////////////////////

/**
 * @return string
 */
function get_lock_file_name()
{
    $lock_directory = get_settings('local', 'lock_directory');
    $lock_file = $lock_directory . '/asterisell.lock';
    return $lock_file;
}

function openLockFile()
{
    $lock_file = get_lock_file_name();

    if (file_exists($lock_file)) {
        $message = "The file \"$lock_file\" is still present before starting the sending of CDRs LOG files to Asterisell server. So the previous run of the procedure did not ended correctly. This is a critical situation. There can be corruptions in the sent data. If the error persist contact the assistance.";
        $isOk = unlink($lock_file);
        if ($isOk == FALSE) {
            $message .= " The file \"$lock_file\" can not be deleted from the procedure. It can be a permission problem. Fix it, otherwise this error message will be always generated. ";
        }
        signalError($message);
    }

    $fh = fopen($lock_file, 'w');
    if ($fh == FALSE) {
        signalError("The file \"$lock_file\" can not be created from the procedure. It can be a permission problem. Fix it, otherwise this error message will be always generated. ");
    } else {
        fclose($fh);
    }
}

function closeLockFile()
{
    $lock_file = get_lock_file_name();

    if (file_exists($lock_file)) {
        $isOk = unlink($lock_file);
        if ($isOk == FALSE) {
            $message = "The file \"$lock_file\" can not be deleted from the procedure. It can be a permission problem. Fix it, otherwise this error message will be always generated. As conseguence of this problem, an error regarding an unexpected interrupt of the procedure will be raised at next run.";
            signalError($message);
        }
    } else {
        // there were a problem creating the lock file.
        // Problem already signaled during open.
    }
}

/////////////////////////////
// GENERATE CONFIGURATIONS //
/////////////////////////////

/**
 * @return string the name of the initial file storing the configuration settings.
 * NOTE: it's name can change during final installation, but initially its name is derived from
 * the name of this script, and it is the same name of the script with ".ini" suffix.
 */
function initialConfigurationSettingsFileName()
{
    $name = basename(__FILE__);
    $m = array();

    if (preg_match('/^(.+)\.php$/i', $name, $m)) {
        return $m[1] . '.ini';
    } else {
        return $name . '.ini';
    }
}

/**
 * @param string $fileNamePath
 * @return string
 */
function normalizeFileNamePath($fileNamePath)
{
    $l = strlen($fileNamePath);
    $s = str_replace('//', '/', $fileNamePath);
    if (strlen($s) !== $l) {
        return normalizeFileNamePath($s);
    } else {
        return $s;
    }
}

function mandatoryConfigure($cmd)
{
    if ($cmd == FALSE) {
        echo "\nError executing configuration command. Contact the assistance.";
        exit(2);
    }
}

function mkdirAndJumpInIt($dirName)
{
    @mkdir($dirName);
    mandatoryConfigure(chdir($dirName));
}

function cdParentDirectory()
{
    $dir1 = getcwd();
    mandatoryConfigure($dir1);

    $dir2 = dirname($dir1);
    mandatoryConfigure($dir2);

    mandatoryConfigure(chdir($dir2));
}

/**
 * Generate the first phase of the configuration files,
 * containing the params that can be set.
 */
function generateConfigure($osVersion, $voipServerVersion)
{
    global $configureDirectoryName;

    // Set default settings

    // NOTE: every time a new setting is added:
    // - add to this list of settings with default value
    // - customize the value according the $onVersion
    // - add in the generation of .ini file
    // - load it during reading of settings in `generateConfigurations` function
    // - use it in generation of configuration files
    // There is a lot of boilerplate in this.
    // In any case the user can manage all settings in a user readable file.

    $initialConfigurationSettingsFileName = initialConfigurationSettingsFileName();

    $scriptName = basename(__FILE__);

    $remote__webdav_server = "https://192.168.1.181";
    $remote__instance_code = "!! CHANGE-ME !!";
    $remote__password = "!! CHANGE-ME !!";
    $remote__accept_self_signed_certificate_from_webdav_server = "false";
    $local__settings_directory = '/etc/opt/asterisell/';
    $local__settings_file_name = $initialConfigurationSettingsFileName;
    $local__cdrs_log_directory = "/var/spool/asterisell/log/";
    $local__cdrs_log_files = $local__cdrs_log_directory . '*';
    $local__log_postrotate_command = '/bin/kill -HUP `cat /var/run/rsyslogd.pid 2> /dev/null` 2> /dev/null';
    $local__os_version = $osVersion;
    $local__voip_server = $voipServerVersion;
    $local__tmp_directory = "/var/spool/asterisell/tmp/";
    $local__send_queue_directory = "/var/spool/asterisell/to-send/";
    $local__lock_directory = "/var/lock";
    $local__syslog_facility_to_use_for_cdrs = "local4";
    $local__voip_server_cdr_template = '\'\${CDR(clid)}\',\'\${CDR(src)}\',\'\${CDR(dst)}\',\'\${CDR(dcontext)}\',\'\${CDR(channel)}\',\'\${CDR(dstchannel)}\',\'\${CDR(lastapp)}\',\'\${CDR(lastdata)}\',\'\${CDR(start)}\',\'\${CDR(answer)}\',\'\${CDR(end)}\',\'\${CDR(duration)}\',\'\${CDR(billsec)}\',\'\${CDR(disposition)}\',\'\${CDR(amaflags)}\',\'\${CDR(accountcode)}\',\'\${CDR(uniqueid)}\',\'\${CDR(userfield)}\'';
    $errors_reporting__signal_error_using_stderr = "true";
    $errors_reporting__signal_errors_using_syslog = "true";
    $scheduler__how_many_minutes_waiting_before_sending = 15;
    $scheduler__max_random_seconds_before_sending = 120;
    $local__where_store_all_cdrs = "/var/log/asterisk-cdr.log";
    $local__cdrs_log_file_name = "cdrs.asterisk-cdrs__v1";
    $local__script_installation_directory = '/opt/asterisell/';

    // Overwrite settings according OS instance

    $configureOSWarning = "\nYou are using a supported Operating System. There should be no problems.";
    $configureVoIPWarning = "\nYou are using a supported VoIP server. There should be no problems.";

    switch ($osVersion) {
        case "generic-linux":
            $configureOSWarning = "\nYou are using the configurations for a generic Operating System. Review them carefully, adapting to your distribution. In case of problems contact the assistance.";
            break;
        case "redhat5":
            break;
        case "redhat6":
            break;
        case "centos5":
            break;
        case "centos6":
            break;
        default:
            echo "\nUnknown OS-VERSION \"$osVersion\"\n";
            exit(2);
    }

    switch ($voipServerVersion) {
        case "generic":
            $configureVoIPWarning = "\nYou are using the configurations for a generic VoIP Server. Review them carefully, adapting to your distribution. In case of problems contact the assistance.";
            break;
        case "asterisk":
            break;
        default:
            echo "\nUnknown VOIP-SERVER-VERSION \"$voipServerVersion\"\n";
            exit(2);
    }

    // Create main configuration file

    mkdirAndJumpInIt($configureDirectoryName);

    // This configuration file is read from the PHP procedure before run, and it contains all the params,
    // that can depend from the user, or from the OS. Params in this file are read-only.

    $configFile = $initialConfigurationSettingsFileName;
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";

    $fileContent = <<<FILE_CONTENT

;;
;; Settings for
;;
;;  $scriptName
;;
;; procedure.
;;
;; NOTE: the majority of these params can be changed only before calling
;;
;;   php $scriptName generate
;;
;; and *not after*
;;
;;   php $scriptName install
;;
;; because the generated configuration files use hard-coded directory names,
;; derived from the params in this file.
;;
;; Modifying this file after the "generate" phase,
;; you can corrupt the behavior of the script.
;;
;; The comments in this file will specify the few parameters that can be changed
;; also after installation.
;;

[remote]

;; The webdav server where sending the files.
;;
;; NOTE: this parameter can be changed in any moment, also after the installation.
webdav_server = "$remote__webdav_server"

;; false for communicating only with webdav_server having a certificated registered
;; with a known and valid CA.
;; true for accepting self signed certificates.
;; Use true only for code testing internal communications between servers.
;;
;; NOTE: this parameter can be changed in any moment, also after the installation.
accept_self_signed_certificate_from_webdav_server = $remote__accept_self_signed_certificate_from_webdav_server

;; NOTE: this parameter can be changed in any moment, also after the installation.
instance_code = "$remote__instance_code"

;; NOTE: this parameter can be changed in any moment, also after the installation.
password = "$remote__password"

[local]

os_version = "$local__os_version"

voip_server = "$local__voip_server"

;; Where this script is installated.
script_installation_directory = "$local__script_installation_directory"

;; Where the settings are installated.
settings_directory = $local__settings_directory

;; The name of the file with the settings.
settings_file_name = $local__settings_file_name

;; Where the Syslog put LOG files with the CDRs data, that is explicitely owned from this procedure.
cdrs_log_directory = "$local__cdrs_log_directory"

;; Syslog can use facilities from local0 to local7.
;; Make sure to use a facility not used in other settings.
;; The log level will be ".info"
syslog_facility_to_use_for_cdrs = "$local__syslog_facility_to_use_for_cdrs"

;; The format of CDRs put in the LOG file from the VoIP server.
;; The format of this setting in related to the VoIP server specification.
;;
;; IMPORTANT: if the VoIP server uses $ in templates, you must use in this string
;; the \$ symbol. It will be transformed into regular $ during parsing phase of the INI file.
voip_server_cdr_template = "$local__voip_server_cdr_template"

;; Write in this file all the CDRs generated from the VoIP server.
;; The content of this file should be inside a directory under log-rotate,
;; otherwise its content can grow too much.
;;
;; NOTE: generated CDRs are sent to this file, and also copied in chunk files,
;; that are sent to Asterisell server, and then deleted. So this is a reference copy of generated CDRs,
;; to store on the local server.
where_store_all_cdrs = "$local__where_store_all_cdrs"

;; This file name is something like "logicalSourceCode-physicalFormatVariant.log"
;; and it is where CDRs to process are stored/queued.
;; This file will contain only the CDRs not already sent to the server,
;; so its size is rather constant.
cdrs_log_file_name = "$local__cdrs_log_file_name"

;; The command to execute after rotating CDRs log files, in order to update the Syslog collecting the CDRs,
;; with the new file descriptor.
log_postrotate_command = "$local__log_postrotate_command"

;; In this directory logrotate put the LOG files, before processing them and moving
;; them into the queue directory. The content of this directory must be preserved also
;; after a boot, so "/tmp" directory is not a good candidate.
;; The content of this directory is cleaned from the routine.
;;
;; IMPORTANT: in order to use atomic file system operations, this directory must be on the same
;; file system/partition of "cdrs_log_directory", and "send_queue_directory"
tmp_directory = "$local__tmp_directory"

;; In this directory there will be the files to send to the remote Asterisell server.
;; The content of this directory is cleaned from the routine.
send_queue_directory = "$local__send_queue_directory"

;; In this directory there is a lock file, signaling when the routine interrupted unexpectedly.
lock_directory = "$local__lock_directory"

[errors_reporting]

;; true for writing error messages (also) to stderror stream,
;; so the cron daemon (if properly configured), con email them.
;;
;; NOTE: this parameter can be changed in any moment, also after the installation.
signal_error_using_stderr = $errors_reporting__signal_error_using_stderr

;; true for writing error messages (also) to syslog, directing to the LOG_CRON facility.
;;
;; NOTE: the routine can both write to syslog and stderr.
;;
;; NOTE: this parameter can be changed in any moment, also after the installation.
signal_errors_using_syslog = $errors_reporting__signal_errors_using_syslog

[scheduler]

;; How often sending the files to the server.
;; Note that the files are not processed immediately on the server side,
;; so there can be a greather delay before seeing the sent calls.
;; Please leave the suggested value, in order to not stress the remote Asterisell server.
;; Resource usage will be monitored on the server side.
how_many_minutes_waiting_before_sending = $scheduler__how_many_minutes_waiting_before_sending

;; In order to avoid that all the local hosts call the remote Asterisell server
;; at the same time, the sending routine can wait a random number of seconds
;; before sending the data.
;; This parameter identifies the maximum number of seconds to wait.
;; NOTE: this parameter can be changed in any moment, also after the installation.
max_random_seconds_before_sending = $scheduler__max_random_seconds_before_sending

FILE_CONTENT;

    mandatoryConfigure(file_put_contents($configFile, $fileContent));

    $scriptName = basename(__FILE__);
    $configFile = 'README';
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";

    $fileContent = <<<FILE_CONTENT
OVERVIEW
========

These are the configuration files for $osVersion Operating System,
and $voipServerVersion VoIP server.
$configureOSWarning
$configureVoIPWarning

NEEDED PACKAGES
===============

REDHAT 5/6 and CENTOS 5/6
-------------------------

  yum install curl libcurl php

ONLY FOR REDHAD 5 AND CENTOS5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RSYSLOG is  not the default  Syslog daemon on  CentOS 5 and  RedHat 5.
This utility need it for  receiving and processing files in a reliable
way.  For installing it:

  yum install rsyslog5
  chkconfig rsyslog on
  service rsyslog restart
  service crond reload

In case rsyslog version 3 is installated, the commands for replacing with version 5 are:

  yum shell
  remove rsyslog
  install rsyslog5
  transaction solve
  transaction run

press Ctrl-D for exiting from the yum shell.

On RedHat 6  and CentOS 6 it  is the default Syslog daemon,  and it is
already installed, and it is already a version greather than 5.

GENERIC LINUX
-------------

Follow the instructions of your distributions, for installing packages
for: php, rsyslog, curl.

These Linux daemon must be enabled after boot:

  cron
  rsyslog

They usually are configured to be active, so it should be not a problem.

SETTINGS
========

Inspect and customize the file

  $initialConfigurationSettingsFileName

Then execute

  php $scriptName generate

The configuration  files will be  created in this directory.   You can
inspect them, and in case fix the params on

  $initialConfigurationSettingsFileName

and then regenerate them again, until you are satisfied.

INSTALLATION
============

After the execution of

  php $scriptName generate

this directory will contain all the configuration files.

You can inspect them, and when you are sure of their content, you must
run as root user:

  php $scriptName install

File permissions and access modes will be set correctly.
In particular the file with the passwords, will be readable
only from root user.

In case of already existing files, there will be a confirmation request.

You  had  to reload  the  VoIP  server, and the Syslog daemon,
in  order  to  enable the  new configuration files.

IMPROVEMENTS
============

If  there is  something that  can be  improved in  these configuration
files, you should send an email to

  support@asterisell.com

IMPORTANT:  remove the  password parameter  from  ".ini" configuration
file, before sending it by email.
FILE_CONTENT;
    mandatoryConfigure(file_put_contents($configFile, $fileContent));

    //

    echo "\n\nConfigure phase terminated.\nNow you can read the file \"$configureDirectoryName/$configFile\", that contains the installation instructions.\n";
}

function generateConfigurations()
{

    global $configureDirectoryName;
    global $explicitLogRotateConfFileName;

    $this_script_file_name = basename(__FILE__);

    $initialSettingsCompleteFileName = realpath(normalizeFileNamePath($configureDirectoryName . '/' . initialConfigurationSettingsFileName()));
    load_settings($initialSettingsCompleteFileName);

    $remote__webdav_server = get_settings('remote', 'webdav_server');
    $remote__instance_code = get_settings('remote', 'instance_code');
    $remote__password = get_settings('remote', 'password');
    $remote__accept_self_signed_certificate_from_webdav_server = get_settings('remote', 'accept_self_signed_certificate_from_webdav_server');
    $local__cdrs_log_directory = get_settings('local', 'cdrs_log_directory');
    $local__cdrs_log_files = normalizeFileNamePath($local__cdrs_log_directory . '/*');
    $local__log_postrotate_command = get_settings('local', 'log_postrotate_command');
    $local__os_version = get_settings('local', 'os_version');
    $local__voip_server = get_settings('local', 'voip_server');
    $local__tmp_directory = get_settings('local', 'tmp_directory');
    $local__send_queue_directory = get_settings('local', 'send_queue_directory');
    $local__lock_directory = get_settings('local', 'lock_directory');
    $local__syslog_facility_to_use_for_cdrs = get_settings('local', 'syslog_facility_to_use_for_cdrs');
    $local__voip_server_cdr_template = get_settings('local', 'voip_server_cdr_template');
    $errors_reporting__signal_error_using_stderr = get_settings('errors_reporting', 'signal_error_using_stderr');
    $errors_reporting__signal_errors_using_syslog = get_settings('errors_reporting', 'signal_errors_using_syslog');
    $scheduler__how_many_minutes_waiting_before_sending = get_settings('scheduler', 'how_many_minutes_waiting_before_sending');
    $scheduler__max_random_seconds_before_sending = get_settings('scheduler', 'max_random_seconds_before_sending');
    $local__where_store_all_cdrs = get_settings('local', 'where_store_all_cdrs');
    $local__cdrs_log_file_name = get_settings('local', 'cdrs_log_file_name');
    $local__complete_cdrs_log_file_name = normalizeFileNamePath($local__cdrs_log_directory . '/' . $local__cdrs_log_file_name);
    $local__script_installation_directory = get_settings('local', 'script_installation_directory');
    $local__script_complete_name = $local__script_installation_directory . $this_script_file_name;
    $local__settings_directory = get_settings('local', 'settings_directory');
    $local__settings_file_name = get_settings('local', 'settings_file_name');
    $local__setting_complete_file_name = normalizeFileNamePath($local__settings_directory . '/' . $local__settings_file_name);

    //

    mkdirAndJumpInIt($configureDirectoryName);

    //

    mkdirAndJumpInIt('etc');
    mkdirAndJumpInIt('asterisk');
    $configFile = 'cdr_syslog.conf';
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";
    $fileContent = <<<FILE_CONTENT

; --- Asterisk Call Detail Records (CDR) - Syslog Backend ---

; IMPORTANT: copying this configuration file into VoIP server,
; can overwrite previous configurations.
; So check the current settings, and in case adapt this file.

[cdr]
facility=$local__syslog_facility_to_use_for_cdrs
priority=info
template = $local__voip_server_cdr_template

FILE_CONTENT;

    mandatoryConfigure(file_put_contents($configFile, $fileContent));
    mandatoryConfigure(chmod($configFile, 0644));

    //

    cdParentDirectory();
    mkdirAndJumpInIt('rsyslog.d');
    $configFile = 'asterisell.conf';
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";
    $fileContent = <<<FILE_CONTENT
#
# Intercept the LOG messages generated from the VoIP server,
# and manage them.
#
# NOTE: "use localX" instead of "local4" according the value used in /etc/asterisk/cdr_syslog.conf
#

# Send CDRs to a file containing all the LOG files of the VoIP Server. This is optional, and it is not used from Asterisell
# The generated LOG file, should be processed from a scheduled logrotate process. Usually all the files in "log"
# directory are managed in this way.
# NOTE: generated CDRs are sent to this file, and also copied in chunk files,
# that are sent to Asterisell server, and then deleted. So this is a reference copy of generated CDRs,
# to store on the local server.
$local__syslog_facility_to_use_for_cdrs.info                   $local__where_store_all_cdrs

# Send CDRs to a working queue.
# Then a cron scheduled job will remove CDRs from this queue,
# and send them to the Asterisell server.
# The file name format is something like "logicalSourceCode__physicalFormatVariant.log"
$local__syslog_facility_to_use_for_cdrs.info                   $local__complete_cdrs_log_file_name

FILE_CONTENT;

    mandatoryConfigure(file_put_contents($configFile, $fileContent));
    mandatoryConfigure(chmod($configFile, 0644));

    //

    cdParentDirectory();
    mkdirAndJumpInIt('cron.d');
    $configFile = 'asterisell';
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";
    $fileContent = <<<FILE_CONTENT

# Send CDRs every $scheduler__how_many_minutes_waiting_before_sending minutes.
# Only the output on stderr is sent to cron daemon.
*/$scheduler__how_many_minutes_waiting_before_sending * * * * root nice php $local__script_complete_name run $local__setting_complete_file_name > /dev/null

FILE_CONTENT;

    mandatoryConfigure(file_put_contents($configFile, $fileContent));
    mandatoryConfigure(chmod($configFile, 0644));

    //

    cdParentDirectory();
    cdParentDirectory();
    $nestLevel = 0;
    $sourceFile = $local__settings_file_name;
    $configFile = $local__settings_file_name;
    foreach (explode("/", $local__settings_directory) as $d) {
        if (trim($d) != '') {
            mkdirAndJumpInIt($d);
            $nestLevel++;
            $sourceFile = '../' . $sourceFile;
        }
    }
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";
    mandatoryConfigure(copy($sourceFile, $configFile));
    mandatoryConfigure(chmod($configFile, 0600));
    // NOTE: this is the password file, and only the root user can read it.

    //

    for ($i = 0; $i < $nestLevel; $i++) {
        cdParentDirectory();
    }

    //

    $nestLevel = 0;
    $sourceFile = realpath(__FILE__);
    $configFile = $this_script_file_name;
    foreach (explode("/", $local__script_installation_directory) as $d) {
        if (trim($d) != '') {
            mkdirAndJumpInIt($d);
            $nestLevel++;
        }
    }
    echo "\nGenerating \"" . getcwd() . "/$configFile\".";
    mandatoryConfigure(copy($sourceFile, $configFile));
    mandatoryConfigure(chmod($configFile, 0644));

    for ($i = 0; $i < $nestLevel; $i++) {
        cdParentDirectory();
    }

    //

    echo "\n";

}

function createInstallDirectory($dir)
{
    $dir = normalizeFileNamePath($dir);

    if (is_dir($dir)) {
        echo "\nDirectory \"$dir\" already exists.";
    } else {
        echo "\nCreating directory \"$dir\".";
        mandatoryConfigure(mkdir($dir, 0755, true));
    }
}

/**
 * @param string $sourceBase the base directory
 * @param string $source $base . '/' . $source is the directory to copy
 * @param string $destinationBase $destinationBase . '/' . $source is the destination
 * @param int $nestLevel
 */
function recursiveInstallationOfFiles($sourceBase, $source, $destinationBase, $nestLevel)
{
    global $input_line;

    $fullSource = normalizeFileNamePath($sourceBase . '/' . $source);

    if (is_dir($fullSource)) {
        $fullDestinationDir = normalizeFileNamePath($destinationBase . '/' . $source);
        createInstallDirectory($fullDestinationDir);

        $handle = opendir($fullSource);
        while (($file = readdir($handle)) !== false) {
            if (strpos($file, '.') !== 0) {
                recursiveInstallationOfFiles($sourceBase, normalizeFileNamePath($source . '/' . $file), $destinationBase, $nestLevel + 1);
            }
        }
        closedir($handle);
    } else {
        if ($nestLevel > 1) {

            $fullDest = normalizeFileNamePath($destinationBase . '/' . $source);

            echo "\nCopying file \"$fullSource\" to \"$fullDest\" ";

            if (file_exists($fullDest)) {
                echo "\nFile \"$fullDest\" already exists. Do you want overwrite it? [y/N]";
                $next_line = trim(fgets($input_line, 1024));
                if ($next_line === "y" || $next_line === "Y") {
                    $copyFile = true;
                } else {
                    $copyFile = false;
                }
            } else {
                $copyFile = true;
            }

            if ($copyFile) {
                mandatoryConfigure(copy($fullSource, $fullDest));
                mandatoryConfigure(chown($fullDest, 'root'));
                $perms = fileperms($fullSource);
                mandatoryConfigure(chmod($fullDest, $perms));
            }
        }
    }
}

function testFileRename()
{
    $doFlush = false;

    $fileName1 = 'asterisell_test1.txt';
    $fileName2 = 'asterisell_test2.txt';

    $f1 = fopen($fileName1, 'a');
    mandatoryConfigure(fwrite($f1, "1\n"));
    if ($doFlush) {
        fflush($f1);
    }

    mandatoryConfigure(rename($fileName1, $fileName2));

    mandatoryConfigure(fwrite($f1, "2\n"));
    mandatoryConfigure(fclose($f1));

    $f1 = fopen($fileName1, 'a');
    mandatoryConfigure(fwrite($f1, "3\n"));
    if ($doFlush) {
        fflush($f1);
    }

    mandatoryConfigure(fclose($f1));
}

/**
 * Install configuration files.
 */
function installConfigurations()
{
    global $configureDirectoryName;

    $initialSettingsCompleteFileName = realpath(normalizeFileNamePath($configureDirectoryName . '/' . initialConfigurationSettingsFileName()));
    load_settings($initialSettingsCompleteFileName);

    createInstallDirectory(get_settings('local', 'tmp_directory'));
    createInstallDirectory(get_settings('local', 'send_queue_directory'));
    createInstallDirectory(get_settings('local', 'lock_directory'));
    createInstallDirectory(get_settings('local', 'cdrs_log_directory'));

    mkdirAndJumpInIt($configureDirectoryName);
    recursiveInstallationOfFiles(getcwd(), '', '/', 0);

    echo "\nMake sure to restart/reload these services: rsyslog, crond, VoIP server.\n";
}


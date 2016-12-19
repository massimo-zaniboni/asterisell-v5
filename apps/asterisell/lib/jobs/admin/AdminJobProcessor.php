<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A Job that is executed for configuring or upgrading the application.
 * See `app.yml` for more informations.
 * These jobs are activated from the console management.
 *
 * The `process` function can return info message, or generate an Exception.
 */
abstract class AdminJobProcessor extends FixedJobProcessor
{

    /**
     * @const boolean true for displaying the sent commands in debug mode
     */
    const DEBUG_MODE = false;

    const SHOW_ONLY_ERROR_DEBUG_MODE = true;

    /**
     * @return bool true for job that upgrade the DB and it must be executed first to reinit the triggers and stored procedures.
     * false for a job changing data on the instance, and that needs a good DB model, and updated triggers.
     */
    public function isDBUpgradeJob() {
        return false;
    }

    /**
     * `instance_code_name` value on `app.yml`, in case the job is parametric on it.
     *
     * @return string
     */
    protected function getInstanceCodeName()
    {
        return trim(getInstanceConfigValue('instance_code_name'));
    }

    /**
     * `instance_code_name` value on `app.yml`, in case the job is parametric on it.
     *
     * @return string
     */
    protected function getInstanceVoipDomain()
    {
        return trim(getInstanceConfigValue('instance_voip_domain'));
    }

    /**
     * Overrides in case the default behavior is not good.
     *
     * @return string a unique upgrade key. If this key is already on the `ar_application_upgrade` table,
     * then the job is considered already executed.
     */
    public function getUpgradeKey()
    {
        return get_class($this);
    }

    /**
     * @abstract
     * @return bool true if this job modify the CDR table, and so the system is locked for maybe a lot of time.
     * NOTE that now with the separation between ArCdr and Cdr, these conditions are rarely or non exists.
     */
    abstract public function isCDRTableModified();


    /**
     * @param string $fileName
     * @return string
     * @throws ArProblemException
     */
    protected function sendSQLProceduresToMySQLFromFile($fileName)
    {
        $sqlCode = file_get_contents($fileName);

        if ($sqlCode === FALSE) {
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::APPLICATION,
                null,
                    get_class($this) . ' - ' . rand(),
                    'Error during upgrading of MySQL stored procedures library. The file "' . $fileName . '" was not found.',
                'The MySQL code will be not updated. This will cause problems in rating procedure. .',
                'This is an error in the application. Contact the assistance.'
            );

            throw($p);

        }

        return $this->sendSQLProceduresToMySQLFromString($sqlCode);
    }

    /**
     * @param string $sqlCode SQL commands separated with $$
     * @return string
     * @throws ArProblemException
     */
    protected function sendSQLProceduresToMySQLFromString($sqlCode)
    {
        $sqlCommands = explode('$$', $sqlCode);

        $lastSQL = '';
        try {
            $conn = Propel::getConnection();
            $conn->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

            foreach ($sqlCommands as $sqlCommand) {
                if (!isEmptyOrNull(trim($sqlCommand))) {
                    if (self::DEBUG_MODE) {
                        echo "\n" . $sqlCommand;
                    }
                    $lastSQL = $sqlCommand;
                    $isOK = $conn->exec($sqlCommand);

                    if ($isOK === FALSE) {
                        throw(new Exception('Error in SQL code'));
                    }

                    if (self::DEBUG_MODE) {
                        self::waitUserConfirmation();
                    }
                }
            }
        } catch (Exception $e) {

            $p = ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e,
                    get_class($this) . ' - ' . rand(),
                    'Error during upgrading of MySQL stored procedures library, on SQL code: ' . "\n\n$lastSQL",
                'The MySQL code will be not updated. This will cause problems in rating procedure. .',
                'This is an error in the application. Contact the assistance.'
            );

            if (self::DEBUG_MODE || self::SHOW_ONLY_ERROR_DEBUG_MODE) {
                echo "\n!!!ERROR: " . $p->getLastErrorDescription();
                if (self::DEBUG_MODE) {
                    self::waitUserConfirmation();
                }
            }

            throw($p);
        }

        return 'Updaded MySQL stored procedures.';
    }

    static protected function waitUserConfirmation()
    {
        static $input_line;

        if (is_null($input_line)) {
            $input_line = fopen('php://stdin', 'r');
        }

        echo "\nPress ENTER for continuing...";

        fgets($input_line, 1024);
    }

    /**
     * @param string $internalName
     * @return ArVendor
     */
    protected function createOrReplaceVendor($internalName) {
        $r = ArVendorPeer::retrieveByInternalName($internalName);
        if (is_null($r)) {
            $r = new ArVendor();
            $r->setInternalName($internalName);
        }
        return $r;
    }

    /**
     * @param string $internalName
     * @return ArCommunicationChannelType
     */
    protected function createOrReplaceCommunicationChannel($internalName) {
        $r = ArCommunicationChannelTypePeer::retrieveByInternalName($internalName);
        if (is_null($r)) {
            $r = new ArCommunicationChannelType();
            $r->setInternalName($internalName);
        }
        return $r;
    }

    /**
     * @param string $domain
     * @return ArVendorDomain
     */
    protected function createOrReplaceVendorDomain($domain) {
        $c = new Criteria();
        $c->add(ArVendorDomainPeer::DOMAIN, $domain);
        $r = ArVendorDomainPeer::doSelectOne($c);
        if (is_null($r)) {
            $r = new ArVendorDomain();
            $r->setDomain($domain);
        }
        return $r;
    }

    /**
     * @param string $name
     * @return ArParty
     */
    protected function createOrReplacePartyByName($name) {
        $c = new Criteria();
        $c->add(ArPartyPeer::NAME, $name);
        $r = ArPartyPeer::doSelectOne($c);
        if (is_null($r)) {
            $r = new ArParty();
            $r->setName($name);
        }
        return $r;
    }

}

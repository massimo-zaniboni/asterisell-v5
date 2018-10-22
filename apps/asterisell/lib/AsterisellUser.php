<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * A Asterisell user with proper data access policies.
 *
 * Manage also the application lock during maintanance work.
 *
 * See also `lib/model/ArParty.php` for other info on users.
 */
class AsterisellUser extends sfBasicSecurityUser
{

    ///////////////////////////
    // LOGIN AND CREDENTIALS //
    ///////////////////////////

    /**
     * DEV-NOTE: this value is hard-coded also in fabric management tool. Do not change.
     */
    const APP_LOCK = 'REMOVE_ME_FOR_ENABLING_ASTERISELL_APP';

    /**
     * Init the user.
     *
     * precondition: the user has supplied the correct password
     * and the account is valid
     *
     * @param ArUser $user
     * @return void
     */
    public function login(ArUser $user)
    {
        $this->setAuthenticated(true);
        $this->setAttribute('login', $user->getLogin());
        $this->setAttribute('uniquePrefixCounter', 1);
        $this->setAttribute('uniquePrefixToRateId', array());

        $this->setAttribute('userId', $user->getId());

        $this->setCulture(sfConfig::get('app_culture'));
        $this->clearCredentials();

        if ($user->getIsRootAdmin()) {
            $this->addCredentials('admin', 'user');
            $this->setAttribute('organizationId', null);

            if (sfConfig::get('app_enable_upload_of_files')) {
                $this->addCredentials('can_upload_files');
            }

        } else {
            $this->addCredentials('user');
            $this->setAttribute('organizationId', $user->getArOrganizationUnitId());
        }
    }

    /**
     * @return void
     */
    public function logout()
    {
        $this->setAuthenticated(false);
        $this->clearCredentials();
    }

    /**
     * @return int|null
     */
    public function getUserId()
    {
        return $this->getAttribute('userId');
    }

    /**
     * @return bool
     */
    public function isAdmin()
    {
        if ($this->isAuthenticated()) {
            return $this->hasCredential('admin');
        } else {
            return false;
        }
    }

    /**
     * @return int|null
     */
    public function getParamsId()
    {
        return ArParamsPeer::getDefaultParamsId();
    }

    /**
     * @return null|int the organization for wich the user has roles and permissions
     */
    public function getOrganizationId()
    {
        if ($this->isAuthenticated() && !$this->isAdmin()) {
            return $this->getAttribute('organizationId');
        } else {
            return null;
        }
    }

    /**
     * @return string
     */
    public function getLogin()
    {
        return $this->getAttribute('login');
    }

    /**
     * @return string
     */
    public function getLoginDescription()
    {
        return $this->getLogin();
    }

    /**
     * @return string
     */
    public function getLanguage()
    {
        return sfConfig::get('app_culture');
    }

    /**
     * @return string
     */
    public function getDateFormat()
    {
        return $this->getAttribute('date_format');
    }

    // Go to proper module according user type
    /**
     * Select the best type of call report according the characteristics of the account.
     *
     * @return string|null
     */
    public function getCallReportModuleName()
    {

        if ($this->hasCredential('admin')) {
            if (isStatusServerInstance()) {
                return 'instance_status/list';
            } else {
                return 'admin_call_report/list';
            }
        } else if ($this->hasCredential('user')) {
            return 'customer_call_report/list';
        } else {
            return null;
        }
    }


    /////////////
    // Queries //
    /////////////

    /**
     * @return array list($reportsToRead, $totalReports) the reports associated to the user
     */
    public function countReports()
    {
        $userId = $this->getUserId();
        if (is_null($userId)) {
            return array(0, 0);
        } else {

            $conn = Propel::getConnection();

            $stmt = $conn->prepare('
SELECT COUNT(*)
FROM ar_report_to_read
WHERE ar_user_id = ?
AND NOT seen_or_received_from_user
');
            $stmt->execute(array($userId));

            $newDocs = 0;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $newDocs = $rs[0];
            }
            $stmt->closeCursor();

            $stmt = $conn->prepare('
SELECT COUNT(*)
FROM ar_report_to_read
WHERE ar_user_id = ?
');
            $stmt->execute(array($userId));

            $allDocs = 0;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $allDocs = $rs[0];
            }
            $stmt->closeCursor();

            return array($newDocs, $allDocs);
        }
    }

    ////////////////////////
    // APPLICATION STATUS //
    ////////////////////////

    /**
     * In maintenance mode only administrators can use the application.
     *
     * @static
     * @return void
     * @throws ArProblemException
     */
    static public function lockAppForMaintanance()
    {
        self::manageAppLock(true);
    }

    /**
     * @static
     * @return void
     * @throws ArProblemException
     */
    static public function unlockAppForMaintanance()
    {
        self::manageAppLock(false);
    }

    /**
     * Lock only the USER instance.
     * The ADMIN instance is locked during maintenance disabling directly the web server.
     *
     * @param bool $isLock
     * @throws ArProblemException
     */
    static protected function manageAppLock($isLock)
    {
        $fileName = normalizeFileNamePath(getAsterisellCompleteAdminOrUserDirectory(false) . '/' . self::APP_LOCK);

        $h = null;
        if ($isLock) {
            $h = fopen($fileName, "w");
            if (!($h === FALSE)) {
                fclose($h);
            }
        } else {
            if (file_exists($fileName)) {
                $h = unlink($fileName);
            }
        }

        if ($h === FALSE) {
            if ($isLock) {
                $problemDuplicationKey = 'upgrade application no lock - ' . time();
                $problemDescription = "At " . fromUnixTimestampToSymfonyStrDate(time()) . " the application tried to lock for an upgrade process, but it was no possible to write to the file '$fileName'";
                $problemProposedSolution = "This is an error in the application, or in the permissions of directories. Contact the assistance.";
                $problemEffect = "This instance will be not upgraded.";
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw($p);
            } else {
                $problemDuplicationKey = 'upgrade application no unlock - ' . time();
                $problemDescription = "At " . fromUnixTimestampToSymfonyStrDate(time()) . " the application tried to unlock the upgrade process, but it was no possible to delete the file '$fileName'";
                $problemProposedSolution = "This is an error in the application, or in the permisions of directories. Contact the assistance.";
                $problemEffect = "The application will repeat the upgrade process another time, and it will no process real data, in the meantime. ";
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_CRITICAL,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw($p);

            }
        }
    }

    /**
     * @static
     * @return bool true if the application is locked in maintanance mode
     */
    static public function isAppLockedForMaintanance()
    {
        if (isAdminInstance()) {
            return false;
        } else {
            $fileName = normalizeFileNamePath(getAsterisellCompleteAdminOrUserDirectory(false) . '/' . self::APP_LOCK);

            // note: use a fast file operation
            return file_exists($fileName);
        }
    }

    /**
     * Disable cron job processing for maintenance mode.
     *
     * In case of CRON is only the admin instance that is affected.
     *
     * @static
     * @return void
     */
    static public function lockCronForMaintanance()
    {
        $h = fopen(self::cronLockFileName(), "w");
        if ($h === FALSE) {
            echo "\nUnable to lock file " . self::cronLockFileName();
            exit(1);
        } else {
            fwrite($h, 'hello');
            fclose($h);
        }
    }

    /**
     * @static
     * @return void
     */
    static public function unlockCronForMaintanance()
    {
        unlink(self::cronLockFileName());
    }

    /**
     *
     * @static
     * @return bool true if the cron job processor is locked in maintanance mode
     */
    static public function isCronLockedForMaintanance()
    {
        // note: use a fast file operation
        return file_exists(self::cronLockFileName());
    }

    static public function cronLockFileName()
    {
        // NOTE: write in this directory because it is not deleted during upgrading phase
        return normalizeFileNamePath(ImportDataFiles::getAbsoluteParamsDirectory() . '/REMOVE_ME_FOR_ENABLING_ASTERISELL_CRON_JOB_PROCESSOR');
    }

    static public function getMaintananceModeMessage()
    {
        return mytr("Web Site is currently undergoing scheduled maintenance. Sorry for the inconvenience.");
    }

    /**
     * Override the interface method for credential test.
     *
     * When the application is locked in maintance mode,
     * then only administrators have the right to use the application.
     *
     * @param string $credential
     * @return bool
     */
    public function hasCredential($credential)
    {
        if ($credential === "admin") {
            return parent::hasCredential($credential);
        } else {
            if (self::isAppLockedForMaintanance()) {
                return FALSE;
            } else {
                return parent::hasCredential($credential);
            }
        }
    }

    /**
     * @return string|null
     */
    static public function getInitialHTMLLogMessage()
    {
        $conn = Propel::getConnection();
        $stmt = $conn->prepare('
SELECT html_notes_on_the_login_form
FROM ar_params
WHERE is_default = 1
LIMIT 1
');
        $stmt->execute(array());

        $r = null;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }
        $stmt->closeCursor();

        if (!is_null($r)) {
            if (strlen(trim($r)) == 0) {
                return null;
            }
        }

        return $r;
    }
}

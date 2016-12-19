<?php

/* $LICENSE 2009, 2010, 2011, 2012:
 *
 * Copyright (C) 2009, 2010, 2011, 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * A series of CDR related services, that can be customized from Asterisell instances
 * deriving specialized subclasses.
 *
 * It is created using self::getInstance(),
 * that reads from the app.yml file.
 *
 * A user can change the setting in app.yml, and derive a specialized sub class.
 */
class CustomCDRServices
{

    static $cachedInstance = null;

    /**
     * @return CustomCDRServices the value configured in app.yml
     */
    public static function getInstance()
    {
        if (is_null(self::$cachedInstance)) {
            $className = sfConfig::get('app_custom_cdr_services');
            self::$cachedInstance = new $className();
        }

        return self::$cachedInstance;
    }

    ////////////////////////////////////////
    // METHODS CALLED DURING INSTALLATION //
    ////////////////////////////////////////

    /**
     * Execute some operations not requiring the database,
     * but only correct configuration files.
     * Done these instructions only during initial installation phase.
     *
     * @return bool true if the actions are correctly exeucted, false otherwise.
     */
    public function calledFromInitialInstallation() {

    }

    /**
     * Execute some operations not requiring the database,
     * but only configuration files, during activation phase.
     */
    public function calledFromMakeActivate()
    {

    }

    /**
     * The job creating rate formats, with their description.
     *
     * @return AdminJobProcessor a job adding/replacing rate formats.
     */
    public function getRateFormatsJob() {
        return new InitRateFormats();
    }

    ///////////////////////////////////////
    // INTERFACES THAT CAN BE CUSTOMIZED //
    ///////////////////////////////////////

    /**
     * Retrieve the ArCdrProvider.
     *
     * @param string $cdrProvider
     * @return int|null ArCdrProvider.id
     */
    public function getCdrProviderId($cdrProvider) {
        $l = ArCdrProviderPeer::retrieveByName($cdrProvider);

        if (is_null($l)) {
            return null;
        } else {
            return $l->getId();
        }
    }

    /**
     * Retrieve or create logical type from the database.
     *
     * @param string $logicalType
     * @return int ArLogicalSource.id
     */
    public function getLogicalTypeId($logicalType) {
        $l = ArLogicalSourcePeer::retrieveByName($logicalType);
        if (is_null($l)) {
            $l = new ArLogicalSource();
            $l->setName($logicalType);
            $l->save();
        }

        return $l->getId();
    }

    /**
     * Retrieve or create logical and version type from the database.
     *
     * @param string $logicalType
     * @param string $versionType
     * @return int ArPhysicalFormat.id, the source logical type can be retrieved indirectly
     */
    public function getLogicalTypeAndVersionId($logicalType, $versionType) {
        $l = $this->getLogicalTypeId($logicalType);

        $v  = ArPhysicalFormatPeer::retrieveByName($l, $versionType);
        if (is_null($v)) {
            $v = new ArPhysicalFormat();
            $v->setArLogicalSourceId($l);
            $v->setName($versionType);
            $v->save();
        }

        return $v->getId();
    }

    /**
     * Calculate the condition on calldate.
     *
     * @param int|null $fromDate
     * @param int|null $toDate
     * @param array $params where adding the conditions on date
     * @return string
     */
    public function getConditionOnDate($fromDate, $toDate, & $params)
    {

        $condOnDate = ' ';
        if (!is_null($fromDate)) {
            $condOnDate .= ' AND calldate >= ? ';
            $params[] = fromUnixTimestampToMySQLTimestamp($fromDate);
        }

        if (!is_null($toDate)) {
            $condOnDate .= ' AND calldate < ? ';
            $params[] = fromUnixTimestampToMySQLTimestamp($toDate);
        }

        return $condOnDate;
    }

    /**
     * Calculate the CDRs according the type of error.
     *
     * @param int|null $fromDate
     * @param int|null $toDate
     * @param PDO|null $conn
     * @return array from DestinationType to numbers of CDRs with this type of error.
     */
    public function getCDRsWithErrorsByDestinationType($fromDate, $toDate, $conn = null)
    {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        // Retrieve CDRs with errors in the call flow merging phase.

        $mergeParams = array();
        $mergeParams[] = DestinationType::error;

        $condOnDate = $this->getConditionOnDate($fromDate, $toDate, $mergeParams);

        // Retrieve CDRs with errors in the call rating phase

        $query = 'SELECT error_destination_type, sum(count_of_calls)
                  FROM   ar_cdr
                  WHERE  destination_type = ? '
                . $condOnDate
                . ' GROUP BY error_destination_type';

        $stmt = $conn->prepare($query);
        $stmt->execute($mergeParams);

        $r = array();
        while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
            $errorType = intval($rs[0]);
            $count = intval($rs[1]);

            $r[$errorType] = $count;
        }
        $stmt->closeCursor();

        return $r;
    }

    /**
     * Calculate the CDRs correctly rated, grouped by type of destination type.
     *
     * @param int|null $fromDate
     * @param int|null $toDate
     * @param PDO|null $conn
     * @return array from DestinationType to numbers of CDRs with this type of error.
     */
    public function getRatedCDRsByDestinationType($fromDate, $toDate, $conn = null)
    {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $mergeParams = array();
        $mergeParams[] = DestinationType::error;

        $condOnDate = $this->getConditionOnDate($fromDate, $toDate, $mergeParams);

        $query = 'SELECT destination_type, sum(count_of_calls)
                  FROM   ar_cdr
                  WHERE  destination_type <> ? '
                . $condOnDate
                . ' GROUP BY destination_type';

        $stmt = $conn->prepare($query);
        $stmt->execute($mergeParams);

        $r = array();
        while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
            $destType = intval($rs[0]);
            $count = intval($rs[1]);

            $r[$destType] = $count;
        }
        $stmt->closeCursor();

        return $r;
    }

    /**
     * @param array $errorsByDirection array from DestinationType to number of errors
     * @return int ArProblemType::TYPE_ERROR or similar constant
     */
    public function getProblemType($errorsByDirection)
    {
        if (getFromStats($errorsByDirection, DestinationType::error) > 0) {
            $errorType = ArProblemType::TYPE_ERROR;
        } else if (getFromStats($errorsByDirection, DestinationType::system) > 0) {
            $errorType = ArProblemType::TYPE_ERROR;
        } else if (getFromStats($errorsByDirection, DestinationType::outgoing) > 0) {
            $errorType = ArProblemType::TYPE_ERROR;
        } else if (getFromStats($errorsByDirection, DestinationType::incoming) > 0) {
            $errorType = ArProblemType::TYPE_WARNING;
        } else {
            $errorType = ArProblemType::TYPE_INFO;
        }
        return $errorType;
    }

    /**
     * @param array $errorsByDirection array from DestinationType to number of errors
     * @param array|null $ratedByDirection
     * @param string|null $insertLink
     * @return string|null a user friendly description of the error, null if there are no errors
     */
    public function getErrorDescription($errorsByDirection, $ratedByDirection = null, $insertLink = null)
    {
        $r = array();
        $r[] = $this->getErrorDescriptionEntry($errorsByDirection, $ratedByDirection, DestinationType::error, $insertLink);
        $r[] = $this->getErrorDescriptionEntry($errorsByDirection, $ratedByDirection, DestinationType::outgoing, $insertLink);
        $r[] = $this->getErrorDescriptionEntry($errorsByDirection, $ratedByDirection, DestinationType::system, $insertLink);
        $r[] = $this->getErrorDescriptionEntry($errorsByDirection, $ratedByDirection, DestinationType::incoming, $insertLink);
        $r[] = $this->getErrorDescriptionEntry($errorsByDirection, $ratedByDirection, DestinationType::internal, $insertLink);

        $r2 = array();
        foreach ($r as $rr) {
            if (!isEmptyOrNull($rr)) {
                $r2[] = $rr;
            }
        }

        if (count($r2) > 0) {
            return 'there are ' . implode(', ', $r2);
        } else {
            return null;
        }
    }

    /**
     * @param array $errorsByDirection
     * @param array|null $ratedByDirection
     * @param string $dest
     * @param string|null $insertLink
     * @return string
     */
    protected function getErrorDescriptionEntry($errorsByDirection, $ratedByDirection, $dest, $insertLink = null)
    {
        $countBadCalls = getFromStats($errorsByDirection, $dest);

        if ($dest == DestinationType::error) {
            $destDescr = 'undefined';
            $isUndef = true;
        } else {
            $destDescr = DestinationType::getUntraslatedName($dest);
            $isUndef = false;
        }

        $perc = '';
        if (!is_null($ratedByDirection)) {
            $countGoodCalls = getFromStats($ratedByDirection, $dest);

            if ($isUndef) {


                $tot = 0;
                foreach ($errorsByDirection as $v) {
                    $tot += $v;
                }

                foreach ($ratedByDirection as $v) {
                    $tot += $v;
                }

                $descrPerc = "of all calls";

            } else {
                $tot = $countBadCalls + $countGoodCalls;
                $descrPerc = "of $destDescr calls";
            }

            $perc = '(' . getStatsPerc($countBadCalls, $tot) . "% $descrPerc)";
        }

        if ($countBadCalls > 0) {
            if (!is_null($insertLink)) {
                $countDescr = '<a href="' . url_for($insertLink, TRUE) . '">' . $countBadCalls . '</a>';
            } else {
                $countDescr = $countBadCalls;
            }

            return "$countDescr unrated $destDescr calls $perc";
        } else {
            return '';
        }
    }

    public function getLastDaysToConsiderForInformingOnCDRsWithErrors()
    {
        return 70;
    }

    public function getDateFromWichCDRRatingIsStable()
    {
        return null;
    }

    /**
     * @return string
     */
    public function processInformingOfCDRsWithErrors()
    {
        $lastDays = $this->getLastDaysToConsiderForInformingOnCDRsWithErrors();

        $fromDate = strtotime("-$lastDays days");

        if (!is_null($this->getDateFromWichCDRRatingIsStable())) {
            if ($fromDate < strtotime($this->getDateFromWichCDRRatingIsStable())) {
                $fromDate = strtotime($this->getDateFromWichCDRRatingIsStable());
            }
        }

        $errorsByDirection = $this->getCDRsWithErrorsByDestinationType($fromDate, null);
        $ratedByDirection = $this->getRatedCDRsByDestinationType($fromDate, null);

        $tot = 0;
        foreach ($errorsByDirection as $v) {
            $tot += $v;
        }

        $logKey = getCDRKeyUsingLogIncrement($tot);

        $errorType = $this->getProblemType($errorsByDirection);

        if ($errorType == ArProblemType::TYPE_INFO) {
            $problemEffect = "No problem: this is only an informative message.";
            $problemProposedSolution = "";
        } else {
            $problemEffect = "The call report, and the billing report, will not contain complete info.";
            $problemProposedSolution = "Solve the problems. They are all listed in the error table.";
        }

        $problemDuplicationKey = get_class($this) . " - errors nr " . $logKey;
        $problemGarbageKey = 'report CDRs with errors';

        $errorDescr = $this->getErrorDescription($errorsByDirection, $ratedByDirection);
        if (is_null($errorDescr)) {
            // there are no error to report
            $errorDescr = 'there are no CDRs with errors.';
        }

        $descr = "From date " . fromUnixTimestampToSymfonyStrTimestamp($fromDate) . " to now, " . $errorDescr;

        ArProblemException::garbageCollect($problemGarbageKey, $fromDate, null);
        ArProblemException::createWithGarbageCollection($errorType, ArProblemDomain::RATES, ArProblemResponsible::ADMIN, $problemDuplicationKey, $problemGarbageKey, null, null, $descr, $problemEffect, $problemProposedSolution);

        return $descr;
    }

    ///////////////////////////////////////////////////////////////
    // CUSTOMIZABLE SERVICES RELATED TO ORGANIZATION HIERARCHIES //
    ///////////////////////////////////////////////////////////////

    /**
     * Store in a local cache the created extensions.
     *
     * @var array extensionId => organizationId
     */
    protected $createdExtensions;

    /**
     * Automatically called at the beginning of rating process.
     * @param PDO $conn
     */
    public function initForAutomaticCreationOfOrganizations(PDO $conn)
    {
        $this->createdExtensions = array();
    }

    /**
     * Automatically called at the end of rating process.
     * @param PDO $conn
     */
    public function endForAutomaticCreationOfOrganizations(PDO $conn)
    {
        // NOTE: no need for
        //
        // > OrganizationUnitInfo::resetCachedValues();
        //
        // because OrganizationUnitInfo cache is invalidated every time it is written.
    }

    /**
     * @param string $rootVoipAccount
     * @param string $extensionCode
     * @param int $calldate
     * @return int|null
     */
    public function getAutomaticallyCreatedOrSearchForExtension($rootVoipAccount, $extensionCode, $calldate)
    {
        $index = $this->getFullRootAndExtensionCode($rootVoipAccount, $extensionCode);
        if (isset($this->createdExtensions[$index])) {
            return $this->createdExtensions[$index];
        } else {
            return OrganizationUnitInfo::getInstance()->getOrganizationIdForExtensionCode($index, $calldate);
        }
    }

    /**
     * During CDR rating, if there are VoIP accounts not defined in the system,
     * they can be created initially from the application, and then refined from the user.
     *
     * @param string $rootVoipAccount
     * @param PDO $conn
     * @return int created organizationId
     */
    public function createSomeRootOrganizationAssociatedToRootVoipAccount($rootVoipAccount, PDO $conn)
    {

        $u = new ArOrganizationUnit();
        $u->save($conn);

        $p = new ArParty();
        $p->setName('Default for ' . $rootVoipAccount);
        $p->setIsActive(true);
        $p->setIsBillable(true);
        $p->save($conn);

        $s = new ArOrganizationUnitHasStructure();
        $s->setArPartyId($p->getId());
        $s->setArOrganizationUnitId($u->getId());
        $s->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_GENERIC_ORG)->getId());
        $s->setArParentOrganizationUnitId(null);
        $s->setArRateCategoryId(ArRateCategoryPeer::retrieveIdByInternalName(ArRateCategory::ID_FOR_NORMAL));
        $s->setFrom(FixedJobProcessor::getGlobalStartingDateForCDRProcessinng());
        $s->setExists(true);
        $s->save($conn);

        $u2 = new ArOrganizationUnit();
        $u2->save($conn);

        $s = new ArOrganizationUnitHasStructure();
        $s->setArPartyId(null);
        $s->setExtensionName('default');
        $s->setExtensionCodes('"' . $this->getFullRootAndExtensionCode($rootVoipAccount, '') . '"');
        $s->setExtensionUserCode('');
        $s->setArOrganizationUnitId($u2->getId());
        $s->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId());
        $s->setArParentOrganizationUnitId($u->getId());
        $s->setArRateCategoryId(null);
        $s->setFrom(FixedJobProcessor::getGlobalStartingDateForCDRProcessinng());
        $s->setExists(true);
        $s->save($conn);

        ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_WARNING,
            ArProblemDomain::VOIP_ACCOUNTS,
            null,
            'missing root account definition for ' . $rootVoipAccount,
            "System created a root organization/customer, associated to the VoIP account code \"$rootVoipAccount\"",
            "Related calls are assigned to this organization/customer. But there are missing parameters like real name, real billing address and so on.",
            "Complete the organization/customer info. Warning: this message is generated only one time. So if you delete it, you will no advised another time.");

        $this->createdExtensions[$rootVoipAccount] = $u->getId();
        return $u->getId();
    }

    /**
     * @param int $rootOrganizationId
     * @param string $rootVoipAccount
     * @param string $extensionCode
     * @param PDO $conn
     * @return int created extensionId
     */
    public function createSomeExtensionForRootVoipAccount($rootOrganizationId, $rootVoipAccount, $extensionCode, PDO $conn)
    {

        $rootParentName = OrganizationUnitInfo::getInstance()->getFullNameAtDate($rootOrganizationId, null, false, false, null, false);

        $completeExtensionCode = $this->getFullRootAndExtensionCode($rootVoipAccount, $extensionCode);

        $u = new ArOrganizationUnit();
        $u->save($conn);

        $s = new ArOrganizationUnitHasStructure();
        $s->setArPartyId(null);
        $s->setExtensionName($extensionCode);
        $s->setExtensionCodes('"' . $completeExtensionCode . '"');
        $s->setArOrganizationUnitId($u->getId());
        $s->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId());
        $s->setArParentOrganizationUnitId($rootOrganizationId);
        $s->setArRateCategoryId(null);
        $s->setFrom(FixedJobProcessor::getGlobalStartingDateForCDRProcessinng());
        $s->setExists(true);
        $s->save($conn);

        ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_WARNING,
            ArProblemDomain::VOIP_ACCOUNTS,
            null,
           'missing extension definition for ' . $completeExtensionCode,
            "System created a VoIP extension \"$completeExtensionCode\".",
            "Related calls are assigned to this extension, but there cane be missing parameters like extension user readable name, and so on.",
            "Complete the extension info. Warning: this message is generated only one time. So if you delete it, you will no advised another time.");


        $this->createdExtensions[$completeExtensionCode] = $u->getId();
        return $u->getId();
    }

    /**
     * @param string $rootVoipAccount
     * @param string $extensionCode
     * @param int $callDate
     * @param PDO $conn
     * @return int created extensionId
     */
    public function getOrCreateSomeExtensionAndMaybeRootOrganization($rootVoipAccount, $extensionCode, $callDate, PDO $conn)
    {
        $extensionId = $this->getAutomaticallyCreatedOrSearchForExtension($rootVoipAccount, $extensionCode, $callDate);
        if (is_null($extensionId)) {
            $rootExtensionId = $this->getAutomaticallyCreatedOrSearchForExtension($rootVoipAccount, '', $callDate);

            if (!is_null($rootExtensionId)) {
                $rootOrganizationId = OrganizationUnitInfo::getInstance()->getParentId($rootExtensionId, $callDate);
                assert(!is_null($rootOrganizationId));
            } else {
                $rootOrganizationId = $this->createSomeRootOrganizationAssociatedToRootVoipAccount($rootVoipAccount, $conn);
            }
            $extensionId = $this->createSomeExtensionForRootVoipAccount($rootOrganizationId, $rootVoipAccount, $extensionCode, $conn);
        }

        return $extensionId;
    }

    /**
     * @param string $rootVoipAccountCode a customer voip account code
     * @param string $extensionCode
     * @return string a unique voip-account-code associated to the customer with the given $rootVoipAccountCode,
     * and $extensionCode
     */
    public function getFullRootAndExtensionCode($rootVoipAccountCode, $extensionCode)
    {
        return $rootVoipAccountCode . '-' . $extensionCode;
    }
}

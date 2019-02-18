<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * Base class for installation, regression tests, and other
 * services of this type.
 */
abstract class InstallationService extends FixedJobProcessor
{

    /**
     * The channel to use for generating demo data.
     */
    const DEMO_CHANNEL = "asterisell-demo-channel";

    /**
     * Create a new organization.
     *
     * @param int|null $parentId
     * @param int $typeId
     * @param string $name
     * @param int|null $arRateCategoryId
     * @param int|null $arPartyId
     * @return int the created ArOrganizationUnit
     */
    protected function createOrganization($parentId, $typeId, $name, $arRateCategoryId, $arPartyId)
    {
        $creationDate = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();

        $u = new ArOrganizationUnit();
        $u->save();

        $uId = $u->getId();

        $s = new ArOrganizationUnitHasStructure();
        $s->setArPartyId($arPartyId);
        $s->setArOrganizationUnitId($uId);
        $s->setArOrganizationUnitTypeId($typeId);
        $s->setArParentOrganizationUnitId($parentId);
        $s->setArRateCategoryId($arRateCategoryId);
        $s->setFrom($creationDate);
        $s->setExists(true);
        $s->save();

        return $uId;
    }

    /**
     * @param string $name
     * @param string $login
     * @param string $password
     * @param string $account
     * @return int ArUser.id
     */
    protected function createResponsibleUser($name, $login, $password, $account)
    {
        $parentId = ArOrganizationUnitPeer::retrieveByInternalName($account)->getId();

        $party = new ArParty();
        $party->setName($name);
        $party->save();

        $user = new ArUser();
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($parentId);
        $user->setIsEnabled(true);
        $user->setLogin($login);
        $user->setClearPassword($password);
        $user->save();

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $rel->save();
    }

    /**
     * @param string $name
     * @param string $code
     * @param string $parentCode
     * @return int ArOrganizationUnit.id
     */
    protected function createCompleteExtension($name, $code, $parentCode)
    {
        $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();
        $parentId = ArOrganizationUnitPeer::retrieveByInternalName($parentCode)->getId();
        return $this->createExtension($parentId, $code, $extensionId, $name);
    }

    /**
     * Create a new extension, associating to a OrganizationUnit of type extension
     *
     * @param int|null $parentId
     * @param string $code
     * @param int $extensionTypeId
     * @param string|null $humanName
     * @return int ArOrganizationUnit.id
     */
    protected function createExtension($parentId, $code, $extensionTypeId, $humanName = null)
    {
        $creationDate = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();

        $u = new ArOrganizationUnit();
        $u->save();

        $s = new ArOrganizationUnitHasStructure();
        $s->setArOrganizationUnitId($u->getId());
        $s->setArOrganizationUnitTypeId($extensionTypeId);
        $s->setArParentOrganizationUnitId($parentId);
        $s->setArRateCategoryId(null);
        $s->setFrom($creationDate);
        $s->setExists(true);

        if (!is_null($humanName)) {
            $s->setExtensionName($humanName);
            $s->setExtensionUserCode($code);
        }
        $s->setExtensionCodes($code);
        $s->save();

        return $u->getId();
    }

    /**
     * @param mixed[] $arr
     * @return mixed a random element of an array
     */
    protected function my_array_rand($arr)
    {
        $k = array_rand($arr);
        return $arr[$k];
    }

    /**
     * @param string $sql
     * @param mixed $expectedResult
     * @return bool
     */
    protected function unitTestOfMySQLFunction($sql, $expectedResult)
    {
        $realResult = null;

        $stmt = Propel::getConnection()->prepare($sql);
        $stmt->execute(array());
        while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
            $realResult = $rs[0];
        }
        $stmt->closeCursor();

        if ($realResult == $expectedResult) {
            // all ok
            return true;
        } else {
            echo "\n!!! query \"$sql\" returned \"$realResult\" instead of expected \"$expectedResult\"";
            return false;
        }
    }

    /**
     * @param string $name
     * @param mixed $realResult
     * @param mixed $expectedResult
     * @return bool
     */
    protected function unitTestOfCondition($name, $realResult, $expectedResult)
    {
        if ($realResult == $expectedResult) {
            // all ok
            echo "\n    SUCCESS: $name";
            return true;
        } else {
            echo "\n!!! condition with name \"$name\" returned \"$realResult\" instead of expected \"$expectedResult\"";
            return false;
        }
    }

    /**
     * @param string $name
     * @param mixed $realResult list($result1, $result2)
     * @param mixed $expectedResult1
     * @param mixed $expectedResult2
     * @return bool
     */
    protected function unitTestOfConditionForPair($name, $realResult, $expectedResult1, $expectedResult2)
    {
        list($result1, $result2) = $realResult;

        $r = true;

        $r = $this->unitTestOfCondition($name . ' (test first result)', $result1, $expectedResult1) && $r;
        $r = $this->unitTestOfCondition($name . ' (test second result)', $result2, $expectedResult2) && $r;
        return $r;
    }

    protected function unitTestOfConditionReturningArrayOfArray($name, $realResult, $expectedResult)
    {
        $realResultDump = print_r($realResult, true);
        $expectedResultDump = print_r($expectedResult, true);

        if ($realResultDump == $expectedResultDump) {
            // all ok
            return true;
        } else {
            echo "\n!!! condition with name \"$name\" returned \"$realResultDump\" instead of expected \"$expectedResultDump\"";
            return false;
        }
    }

    protected function publishReport(ArReport $report, $alreadyReviewed = true)
    {
        $fromDate = $report->getFromDate();
        if (!is_null($fromDate)) {
            $fromDate = fromMySQLTimestampToUnixTimestamp($fromDate);
            $report->setCachedParentIdHierarchy(OrganizationUnitInfo::getInstance()->getFullIds($report->getArOrganizationUnitId(), $fromDate));
            $report->save();
            ArReportPeer::publishReportToUsers($report->getId(), $alreadyReviewed, true, Propel::getConnection());
        }
    }

    /**
     * @param string $testName
     * @param ArUser $user
     * @param ArReport $report
     * @param bool $expectedCanView
     * @return bool
     */
    protected function unitTestIfUserCanViewReport($testName, ArUser $user, ArReport $report, $expectedCanView)
    {

        $c = new Criteria();
        $c->add(ArUserCanViewReportPeer::AR_USER_ID, $user->getId());
        $c->add(ArUserCanViewReportPeer::AR_REPORT_ID, $report->getId());

        $count = ArUserCanViewReportPeer::doCount($c);

        $realCanView = ($count > 0);

        return $this->unitTestOfCondition($testName, $realCanView, $expectedCanView);
    }


}
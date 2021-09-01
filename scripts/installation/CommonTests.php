<?php

// SPDX-License-Identifier: GPL-3.0-or-later

/**
 * Execute common regression tests.
 */
class CommonTests extends InstallationService
{

    ///////////////////
    // JOB INTERFACE //
    ///////////////////

    public function process()
    {

        //
        // Execute the tests, assuming a correctly installed environment.
        //

        echo "\nExecute the tests.";

        $r = true;
        $r = $this->variousUnitTests() && $r;
        $r = $this->unitTestOfMySQLCode() && $r;
        $r = $this->unitTestOfReportWorkflow() && $r;
        $r = $this->unitTestOfOrganizationInfo() && $r;
        $r = RateEngineService::executeRateEngineTests() && $r;
        $r = $this->unitTestOfOrganizationInfoOnTheHaskellSide() && $r;

        // Dispaly manual checks to do

        echo "\nMANUAL CHECKS: use in the code only something like " . '$c->addAscendingOrder(...)->addAscendingOrder(....)' . " due to a bug in Propel.";

        // Final test result

        if ($r) {
            echo "\n\nAll tests are ok.\n";
        } else {
            echo "\n\n!!! There are test with a failure. Check the error messages. \n";
        }

        return '';
    }

    //////////////////////
    // REGRESSION TESTS //
    //////////////////////

    protected function variousUnitTests()
    {

        list($statusName, $minDate, $maxDate) = ImportDataFiles::getTimeFrame(null, null, null);
        if ($statusName != '0000-00-00') {
            return false;
        }
        if (!(is_null($minDate) && is_null($maxDate))) {
            echo "\n   !!! failure on variousUnitTests 1";
            return false;
        }

        list($statusName, $minDate, $maxDate) = ImportDataFiles::getTimeFrame(2014, null, null);
        if ($statusName != '2014-00-00') {
            echo "\n   !!! failure on variousUnitTests 2a: $statusName";
            return false;
        }
        if (!($minDate == strtotime('2014-01-01') && $maxDate == strtotime('2015-01-01'))) {
            echo "\n   !!! failure on variousUnitTests 2b: minDate = " . date('Y-m-d', $minDate) . ", maxDate = " . date('Y-m-d', $maxDate);
            return false;
        }

        list($statusName, $minDate, $maxDate) = ImportDataFiles::getTimeFrame(2014, 11, null);
        if ($statusName != '2014-11-00') {
            echo "\n   !!! failure on variousUnitTests 3";

            return false;
        }
        if (!($minDate == strtotime('2014-11-01') && $maxDate == strtotime('2014-12-01'))) {
            echo "\n   !!! failure on variousUnitTests 4";

            return false;
        }

        list($statusName, $minDate, $maxDate) = ImportDataFiles::getTimeFrame(2014, 12, null);
        if ($statusName != '2014-12-00') {
            echo "\n   !!! failure on variousUnitTests 5";

            return false;
        }
        if (!($minDate == strtotime('2014-12-01') && $maxDate == strtotime('2015-01-01'))) {
            echo "\n   !!! failure on variousUnitTests 6";

            return false;
        }

        list($statusName, $minDate, $maxDate) = ImportDataFiles::getTimeFrame(2014, 12, 31);
        if ($statusName != '2014-12-31') {
            echo "\n   !!! failure on variousUnitTests 7";

            return false;
        }
        if (!($minDate == strtotime('2014-12-31') && $maxDate == strtotime('2015-01-01'))) {
            echo "\n   !!! failure on variousUnitTests 8";

            return false;
        }

        echo "\nTest on timeframe success.";

        // Test Time Frame

        $d1 = strtotime('2015-10-01');
        $d2 = strtotime('2015-10-15');
        $d3 = strtotime('2015-10-05');
        $d4 = strtotime('2015-11-01');

        if (!(FixedJobProcessor::compareDateWithTimeFrame($d3, $d1, $d2) == 0)) {
            echo "\n   !!! failure on variousUnitTests 9.1";
            return false;
        }

        if (!(FixedJobProcessor::compareDateWithTimeFrame($d1, $d2, $d3) == -1)) {
            echo "\n   !!! failure on variousUnitTests 9.2";
            return false;
        }

        if (!(FixedJobProcessor::compareDateWithTimeFrame($d2, $d1, $d3) == 1)) {
            echo "\n   !!! failure on variousUnitTests 9.3";
            return false;
        }

        if (!(FixedJobProcessor::compareDateWithTimeFrame($d2, $d1, null) == 0)) {
            echo "\n   !!! failure on variousUnitTests 9.4";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d1, $d3, $d2, $d4) == false)) {
            echo "\n   !!! failure on variousUnitTests 9.5.1";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d1, null, $d2, $d4) == true)) {
            echo "\n   !!! failure on variousUnitTests 9.5.2";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d1, $d2, $d3, $d4) == true)) {
            echo "\n   !!! failure on variousUnitTests 9.5.3";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d1, $d3, $d2, null) == false)) {
            echo "\n   !!! failure on variousUnitTests 9.5.4";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d1, null, $d3, $d4) == true)) {
            echo "\n   !!! failure on variousUnitTests 9.6";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d1, $d3, $d1, null) == true)) {
            echo "\n   !!! failure on variousUnitTests 9.7";
            return false;
        }

        if (!(FixedJobProcessor::isTimeFrameJoinableWith($d3, $d2, strtotime('2015-10-10'), $d4) == true)) {
            echo "\n   !!! failure on variousUnitTests 9.8";
            return false;
        }

        $s1 = strtotime('2018-01-01');
        for ($i = 0; $i < 48; $i++) {
            $s2 = strtotime('+1 month', $s1);
            if (!(getPreviousMonth($s2) == $s1)) {
              echo "\n   !!! failure on variousUnitTests 9.9";
              return false;
            }
            $s1 = $s2;
        }

        if (!(isPrefixOf("abc", "abcdefg") &&
            isPrefixOf("abc", "abc") &&
            isPrefixOf("", "abc") &&
            isSuffixOf("", "abc") &&
            isSuffixOf("abc", "123abc") &&
            isSuffixOf("abc", "abc") &&
            (!isSuffixOf("abc", "123ab")))
        ) {
            echo "\n   !!! failure on variousUnitTests 9.10";
            return false;
        }

        if (!
             (fromUnixTimestampToWholeDayStart(strtotime('2018-01-01 14:00:00'), true) == strtotime('2018-01-01 00:00:00')
              && fromUnixTimestampToWholeDayEnd(strtotime('2018-01-01 14:00:00'), true) == strtotime('2018-01-02 00:00:00')
              && fromUnixTimestampToWholeDayStart(strtotime('2018-01-01 00:00:00'), false) == strtotime('2018-01-01 00:00:00')
              && fromUnixTimestampToWholeDayStart(strtotime('2018-01-01 14:00:00'), false) == strtotime('2018-01-02 00:00:00')              && fromUnixTimestampToWholeDayStart(strtotime('2018-01-01 00:00:00'), false) == strtotime('2018-01-01 00:00:00')
              && fromUnixTimestampToWholeDayEnd(strtotime('2018-01-01 14:00:00'), false) == strtotime('2018-01-01 00:00:00')
              && fromUnixTimestampToWholeDayEnd(strtotime('2018-01-01 00:00:00'), false) == strtotime('2018-01-01 00:00:00')
             )
           ) {
            echo "\n   !!! failure on variousUnitTests 9.11.1";
            return false;
        }

        if (!
             (fromUnixTimestampToWholeDayStart(strtotime('2018-01-01 14:00:00'), false) == strtotime('2018-01-02 00:00:00')
              && fromUnixTimestampToWholeDayEnd(strtotime('2018-01-01 14:00:00'), false) == strtotime('2018-01-01 00:00:00'))
           ) {
            echo "\n   !!! failure on variousUnitTests 9.11.2";
            return false;
        }

        $t1 = strtotime('2018-01-01 14:00:00');
        $t2 = strtotime('2018-02-02 00:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, $t1, strtotime('2018-01-02 00:00:00'), strtotime('2018-02-02 00:00:00'), false)) {
            echo "\n   !!! failure on variousUnitTests 9.15";
            return false;
        }

        $t1 = strtotime('2018-01-01 14:00:00');
        $t2 = strtotime('2018-01-01 18:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, $t1, $t2, false, false))
        {
            echo "\n   !!! failure on variousUnitTests 9.16";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        $t2 = strtotime('2018-01-01 18:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, $t1, $t2, false, false))
        {
            echo "\n   !!! failure on variousUnitTests 9.17";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        $t2 = strtotime('2018-02-02 18:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, false, $t1, strtotime('2018-02-02 00:00:00'), $t2))
        {
            echo "\n   !!! failure on variousUnitTests 9.18";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        $t2 = strtotime('2018-02-02 00:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, false, $t1, $t2, false))
        {
            echo "\n   !!! failure on variousUnitTests 9.19";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        $t2 = strtotime('2018-02-02 18:00:00');
        $t3 = strtotime('2018-02-02 00:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, false, $t1, $t3, $t2))
        {
            echo "\n   !!! failure on variousUnitTests 9.20";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, null, false, $t1, null, false))
        {
            echo "\n   !!! failure on variousUnitTests 9.20.1";
            return false;
        }

        $t1 = strtotime('2018-01-01 14:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, null, $t1, strtotime('2018-01-02 00:00:00'), null, false))
        {
            echo "\n   !!! failure on variousUnitTests 9.21";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        $t2 = strtotime('2018-01-01 18:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, $t1, strtotime('2018-01-01 18:00:00'), false, false)) {
            echo "\n   !!! failure on variousUnitTests 9.22";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        $t2 = strtotime('2018-01-04 18:00:00');
        $t3 = strtotime('2018-01-04 00:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, $t2, false, $t1, $t3, $t2)) {
            echo "\n   !!! failure on variousUnitTests 9.23";
            return false;
        }

        $t1 = strtotime('2018-01-01 00:00:00');
        if (! $this->testTimeFrameToWholeDay($t1, null, false, strtotime('2018-01-01 00:00:00'), null, false)) {
            echo "\n   !!! failure on variousUnitTests 9.24";
            return false;
        }



        return true;
    }

    /**
     * @param array|null $arr1
     * @param array|null $arr2
     * @return bool true if the two arrays have same values
     */
    protected function testArray($arr1, $arr2) {
        $this->print_arr($arr2);

        if (is_null($arr1) && is_null($arr2)) {
            return true;
        }

        if (is_null($arr1) || is_null($arr2)) {
            return false;
        }

        return $arr1 == $arr2;
    }

    protected function print_arr($arr) {
        if (is_null($arr)) {
            echo "[false]";
        } else {
            echo "[";
            foreach($arr as $v) {
              if (is_null($v)) {
                  echo "null";
              } else {
                  echo fromUnixTimestampToMySQLTimestamp($v);
              }
              echo "  ";
          }
          echo "]";
        }
    }

    /**
     * @param int $fromDate
     * @param int|null|bool $toDate
     * @param int|bool $d1 false if there is no 1 interval
     * @param int|null|bool $d2 false if there is no 2 interval, null for open-time frame
     * @param int|null|bool $d3 false if there is no 2 and 3 interval, null for open-time frame
     * @param int|null|bool $d4 false if there is no 3 interval, null for open-time frame
     * @return bool true if the returned time frame is the same
     */
    protected function testTimeFrameToWholeDay($fromDate, $toDate, $d1, $d2, $d3, $d4) {
        $maybeR = fromTimeFrameToWholeDay($fromDate, $toDate, 0);
        if (! $this->testArray($maybeR, array($fromDate, $toDate))) {
            return false;
        }

        $maybeR = fromTimeFrameToWholeDay($fromDate, $toDate, 1);
        if ($d1 === false || $d2 === false) {
           $expectedR = null;
        } else {
           $expectedR = array($d1, $d2);
        }
        if (! $this->testArray($maybeR, $expectedR)) {
            return false;
        }

        $maybeR = fromTimeFrameToWholeDay($fromDate, $toDate, 2);
        if ($d2 === false || $d3 === false) {
           $expectedR = null;
        } else {
           $expectedR = array($d2, $d3);
        }
        if (! $this->testArray($maybeR, $expectedR)) {
            return false;
        }

        $maybeR = fromTimeFrameToWholeDay($fromDate, $toDate, 3);
        if ($d3 === false || $d4 === false) {
           $expectedR = null;
        } else {
           $expectedR = array($d3, $d4);
        }
        if (! $this->testArray($maybeR, $expectedR)) {
            return false;
        }

        return true;
    }

    /**
     * @return bool true if all tests are ok
     */
    protected function unitTestOfMySQLCode()
    {
        // load last data

        $r = true;

        // TODO refactor

        ///////////////////
        // Test Holidays //
        ///////////////////

        // TODO use tests on Haskell side

        return $r;
    }

    /**
     *
     */
    protected function unitTestOfOrganizationInfo()
    {
        $r = true;

        $i = 0;
        $testPassage = 0;

        $typeId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_GENERIC_ORG)->getId();
        $normalCategoryId = ArRateCategoryPeer::retrieveByInternalName(ArRateCategory::ID_FOR_NORMAL)->getId();

        $r = new ArRateCategory();
        $r->setInternalName(ArRateCategory::ID_FOR_DISCOUNTED);
        $r->save();
        $discountedCategoryId = ArRateCategoryPeer::retrieveByInternalName(ArRateCategory::ID_FOR_DISCOUNTED)->getId();

        $party1 = new ArParty();
        $party1->setName('org1');
        $party1->save();

        $org1 = new ArOrganizationUnit();
        $org1->save();

        $struct1 = new ArOrganizationUnitHasStructure();
        $struct1->setArOrganizationUnitId($org1->getId());
        $struct1->setArOrganizationUnitTypeId($typeId);
        $struct1->setArPartyId($party1->getId());
        $struct1->setArRateCategoryId($normalCategoryId);
        $struct1->setExists(true);
        $struct1->setFrom(strtotime('-12 months'));
        $struct1->setArParentOrganizationUnitId(null);
        $struct1->save();
        // note: after save the cache is invalidated


        // 1
        $i++;
        $info = OrganizationUnitInfo::getInstance()->getDataInfo($org1->getId(), null);
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                $info[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS],
                true
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                $info[OrganizationUnitInfo::DATA_AR_ORGANIZATION_UNIT_ID],
                $org1->getId()
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                $info[OrganizationUnitInfo::DATA_STRUCTURE_ID],
                $struct1->getId()
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getParentId($org1->getId(), null),
                null
            ) && $r;

        // 5
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullNameAtDate($org1->getId(), null, false, false),
                "org1"
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), null),
                "/" . $org1->getId() . "/"
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getNearestRateCategoryId($org1->getId(), null),
                $normalCategoryId
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), null),
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), strtotime('-10 day'))
            ) && $r;

        // add new root organization

        $party2 = new ArParty();
        $party2->setName('org2');
        $party2->save();

        $org2 = new ArOrganizationUnit();
        $org2->save();

        $struct2 = new ArOrganizationUnitHasStructure();
        $struct2->setArPartyId($party2->getId());
        $struct2->setArOrganizationUnitId($org2->getId());
        $struct2->setArOrganizationUnitTypeId($typeId);
        $struct2->setArRateCategoryId($discountedCategoryId);
        $struct2->setExists(true);
        $struct2->setFrom(strtotime('-12 months'));
        $struct2->setArParentOrganizationUnitId(null);
        $struct2->save();

        // change parent

        $struct2 = new ArOrganizationUnitHasStructure();
        $struct2->setArPartyId($party1->getId());
        $struct2->setArOrganizationUnitId($org1->getId());
        $struct2->setArOrganizationUnitTypeId($typeId);
        $struct2->setArRateCategoryId($discountedCategoryId);
        $struct2->setExists(true);
        $struct2->setFrom(strtotime('-6 months'));
        $struct2->setArParentOrganizationUnitId($org2->getId());
        $struct2->save();
        // note: after save the cache is invalidated

        $info = OrganizationUnitInfo::getInstance()->getDataInfo($org1->getId(), null);

        // 9
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                $info[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS],
                true
            ) && $r;

        // 10
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                $info[OrganizationUnitInfo::DATA_AR_ORGANIZATION_UNIT_ID],
                $org1->getId()
            ) && $r;

        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                $info[OrganizationUnitInfo::DATA_STRUCTURE_ID],
                $struct2->getId()
            ) && $r;

        // 12
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getParentId($org1->getId(), null),
                $org2->getId()
            ) && $r;

        // 13
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullNameAtDate($org1->getId(), null, false, false),
                "org2 / org1"
            ) && $r;

        // 14
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullNameAtDate($org1->getId(), strtotime('-8 months'), false, false),
                "org1"
            ) && $r;

        // 15
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), null),
                "/" . $org2->getId() . "/" . $org1->getId() . "/"
            ) && $r;

        // 16
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), strtotime('-8 months')),
                "/" . $org1->getId() . "/"
            ) && $r;

        // 17
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getNearestRateCategoryId($org1->getId(), null),
                $discountedCategoryId
            ) && $r;

        // 18
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getNearestRateCategoryId($org1->getId(), strtotime('-8 months')),
                $normalCategoryId
            ) && $r;

        // 19
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfOrganizationInfo $i",
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), null),
                OrganizationUnitInfo::getInstance()->getFullIds($org1->getId(), strtotime('-10 day'))
            ) && $r;


        // add extension

        $testPrefix = 'regression-test--';

        // remove old test data
        $conn = Propel::getConnection();
        $stmt = $conn->prepare('DELETE FROM ar_organization_unit_has_structure WHERE extension_name = ?');
        $stmt->execute(array($testPrefix));

        $org3 = new ArOrganizationUnit();
        $org3->save();

        $struct3 = new ArOrganizationUnitHasStructure();
        $struct3->setArPartyId(null);
        $struct3->setArOrganizationUnitId($org3->getId());
        $struct3->setArOrganizationUnitTypeId($typeId);
        $struct3->setArRateCategoryId(null);
        $struct3->setExists(true);
        $struct3->setFrom(strtotime('-6 months'));
        $struct3->setArParentOrganizationUnitId(null);
        $struct3->setExtensionName($testPrefix);
        $struct3->setExtensionUserCode($testPrefix . '123');
        $struct3->setExtensionCodes('"' . $testPrefix . '123","' . $testPrefix . '1234"');
        $struct3->save();


        // Test if a changed extension is recognized

        $struct3 = new ArOrganizationUnitHasStructure();
        $struct3->setArPartyId(null);
        $struct3->setArOrganizationUnitId($org3->getId());
        $struct3->setArOrganizationUnitTypeId($typeId);
        $struct3->setArRateCategoryId(null);
        $struct3->setExists(true);
        $struct3->setFrom(strtotime('-30 days'));
        $struct3->setArParentOrganizationUnitId(null);
        $struct3->setExtensionName($testPrefix);
        $struct3->setExtensionUserCode($testPrefix . '456');
        $struct3->setExtensionCodes('"' . $testPrefix . '456","' . $testPrefix . '4567"');
        $struct3->save();

        $oldDate = strtotime('-2 months');
        $recentDate = strtotime('-20 days');


        // Test if a removed extension is not recognized

        $struct3 = new ArOrganizationUnitHasStructure();
        $struct3->setArPartyId(null);
        $struct3->setArOrganizationUnitId($org3->getId());
        $struct3->setArOrganizationUnitTypeId($typeId);
        $struct3->setArRateCategoryId(null);
        $struct3->setExists(true);
        $struct3->setFrom(strtotime('-10 days'));
        $struct3->setArParentOrganizationUnitId(null);
        $struct3->setExtensionName($testPrefix);
        $struct3->setExtensionUserCode($testPrefix . '456');
        $struct3->setExtensionCodes('"' . $testPrefix . '456","' . $testPrefix . '4567"');
        $struct3->save();


        // Test if a removed extension is not recognized

        $struct3 = new ArOrganizationUnitHasStructure();
        $struct3->setArPartyId(null);
        $struct3->setArOrganizationUnitId($org3->getId());
        $struct3->setArOrganizationUnitTypeId($typeId);
        $struct3->setArRateCategoryId(null);
        $struct3->setExists(false);
        $struct3->setFrom(strtotime('-3 days'));
        $struct3->setArParentOrganizationUnitId(null);
        $struct3->setExtensionName(null);
        $struct3->setExtensionUserCode(null);
        $struct3->setExtensionCodes(null);
        $struct3->save();

        $org4 = new ArOrganizationUnit();
        $org4->save();

        $struct3 = new ArOrganizationUnitHasStructure();
        $struct3->setArPartyId(null);
        $struct3->setArOrganizationUnitId($org4->getId());
        $struct3->setArOrganizationUnitTypeId($typeId);
        $struct3->setArRateCategoryId(null);
        $struct3->setExists(true);
        $struct3->setFrom(strtotime('-2 days'));
        $struct3->setArParentOrganizationUnitId(null);
        $struct3->setExtensionName($testPrefix);
        $struct3->setExtensionUserCode($testPrefix . '456');
        $struct3->setExtensionCodes('"' . $testPrefix . '456","' . $testPrefix . '4567"');
        $struct3->save();

        $newNewRecentDate = strtotime('-1 day');
        $badDate = strtotime('-3 day');


        // 48 - test extension conflict
        $i++;

        $org5 = new ArOrganizationUnit();
        $org5->save();

        $struct5 = new ArOrganizationUnitHasStructure();
        $struct5->setArPartyId(null);
        $struct5->setArOrganizationUnitId($org5->getId());
        $struct5->setArOrganizationUnitTypeId($typeId);
        $struct5->setArRateCategoryId(null);
        $struct5->setExists(true);
        $struct5->setFrom(strtotime('-3 months'));
        $struct5->setArParentOrganizationUnitId($org2->getId());
        $struct5->setExtensionName($testPrefix);
        $struct5->setExtensionUserCode($testPrefix . '123');
        $struct5->setExtensionCodes('"' . $testPrefix . '123"');
        $struct5->save();

        return $r;
    }

    /**
     * Create some organization-info data that export to Haskell side,
     * and performs the tests on the Haskell side.
     * The code creating this info must be kept in synchro with the code on the Haskell-side.
     *
     * @return bool true if all test are ok
     */
    protected function unitTestOfOrganizationInfoOnTheHaskellSide()
    {

        //
        // Delete all the data, because I must use fixed ids to pass to the Haskell world
        //

        $tablesToDelete = array(
            'ar_cdr',
            'ar_report_scheduler',
            'ar_report',
            'ar_organization_unit_has_structure',
            'ar_user_has_role',
            'ar_user',
            'ar_organization_unit',
            'ar_organization_unit_type',
            'ar_vendor',
            'ar_party',
            'ar_rate_category',
        );

        foreach ($tablesToDelete as $table) {
            $conn = Propel::getConnection();
            $stmt = $conn->prepare('DELETE FROM ' . $table . ' WHERE true;');
            $stmt->execute(array());
        }


        $exportJob = new ManageRateEvent();

        //
        // Create data to test
        //

        $i = 0;

        $refTime = time();

        $typeId = 1;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_organization_unit_type SET id = ?, name = ?');
        $stm->execute(array($typeId, "org"));

        $normalCategoryId = 1;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_rate_category SET id = ?, short_description = ?, internal_name = ?');
        $stm->execute(array($normalCategoryId, "Normal", ArRateCategory::ID_FOR_NORMAL));

        $discountedCategoryId = 2;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_rate_category SET id = ?, short_description = ?, internal_name = ?');
        $stm->execute(array($discountedCategoryId, "Discounted", ArRateCategory::ID_FOR_DISCOUNTED));

        $party1 = 1;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_party SET id = ?, name = ?');
        $stm->execute(array($party1, "org1"));

        $org1 = 1;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_organization_unit SET id = ?');
        $stm->execute(array($org1));

        $struct1 = 1;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = 1,
        `from` = ?,
        ar_parent_organization_unit_id = ?');
        $stm->execute(array($struct1,
                $org1,
                $typeId,
                $party1,
                $normalCategoryId,
                fromUnixTimestampToMySQLTimestamp(strtotime('-12 months')),
                null)
        );

        $r = true;

        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 0) && $r;

        // add new root organization

        $party2 = 2;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_party SET id = ?, name = ?');
        $stm->execute(array($party2, "org2"));

        $org2 = 2;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_organization_unit SET id = ?');
        $stm->execute(array($org2));

        $struct2 = 2;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = 1,
        `from` = ?,
        ar_parent_organization_unit_id = ?');
        $stm->execute(array(
                $struct2,
                $org2,
                $typeId,
                $party2,
                $discountedCategoryId,
                fromUnixTimestampToMySQLTimestamp(strtotime('-12 months')),
                null)
        );

        // change parent in the future

        $struct2_1 = 2010;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = 1,
        `from` = ?,
        ar_parent_organization_unit_id = ?');
        $stm->execute(array(
                $struct2_1,
                $org1,
                $typeId,
                $party1,
                $discountedCategoryId,
                fromUnixTimestampToMySQLTimestamp(strtotime('-6 months')),
                $org2)
        );

        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 1) && $r;


        // add extension

        $org3 = 3;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_organization_unit SET id = ?');
        $stm->execute(array($org3));

        $structXId = 4;

        $conn = Propel::getConnection();
        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = 1,
        `from` = ?,
        ar_parent_organization_unit_id = ?,
        extension_name = ?,
        extension_user_code = ?,
        extension_codes = ?
        ');
        $stm->execute(array(
                $structXId++,
                $org3,
                $typeId,
                null,
                null,
                fromUnixTimestampToMySQLTimestamp(strtotime('-6 months')),
                null,
                '',
                '123',
                '123,1234'
            )
        );

        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 2) && $r;


        // Test if a changed extension is recognized

        $conn = Propel::getConnection();
        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = 1,
        `from` = ?,
        ar_parent_organization_unit_id = ?,
        extension_name = ?,
        extension_user_code = ?,
        extension_codes = ?
        ');
        $stm->execute(array(
                $structXId++,
                $org3,
                $typeId,
                null,
                null,
                fromUnixTimestampToMySQLTimestamp(strtotime('-30 days')),
                null,
                '',
                '456',
                '456,4567'
            )
        );

        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 3) && $r;

        // Test if a removed extension is not recognized

        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = 1,
        `from` = ?,
        ar_parent_organization_unit_id = ?,
        extension_name = ?,
        extension_user_code = ?,
        extension_codes = ?
        ');
        $stm->execute(array(
                $structXId++,
                $org3, // organization id
                $typeId, // type id
                null, // party
                null, // rate category
                fromUnixTimestampToMySQLTimestamp(strtotime('-10 days')),
                null, // parent organization id
                '', // extension complete name
                '456', // extension short name
                '456,4567' // extension codes
            )
        );

        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 4) && $r;

        // Test if a removed extension is not recognized

        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = ?,
        `from` = ?,
        ar_parent_organization_unit_id = ?,
        extension_name = ?,
        extension_user_code = ?,
        extension_codes = ?
        ');
        $stm->execute(array(
                $structXId++,
                $org3, // organization id
                $typeId, // type id
                null, // party
                null, // rate category
                0, // exists
                fromUnixTimestampToMySQLTimestamp(strtotime('-4 days')),
                null, // parent organization id
                null, // extension complete name
                null, // extension short name
                null // extension codes
            )
        );

        $org4 = 4;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_organization_unit SET id = ?');
        $stm->execute(array($org4));

        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = ?,
        `from` = ?,
        ar_parent_organization_unit_id = ?,
        extension_name = ?,
        extension_user_code = ?,
        extension_codes = ?
        ');
        $stm->execute(array(
                $structXId++,
                $org4, // organization id
                $typeId, // type id
                null, // party
                null, // rate category
                1, // exists
                fromUnixTimestampToMySQLTimestamp(strtotime('-3 days')),
                null, // parent organization id
                null, // extension complete name
                '456', // extension short name
                '456,4567' // extension codes
            )
        );

        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 5) && $r;

        // 48 - test extension conflict

        $org5 = 5;
        $conn = Propel::getConnection();
        $stm = $conn->prepare('INSERT INTO ar_organization_unit SET id = ?');
        $stm->execute(array($org5));


        $stm = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET id = ?,
        ar_organization_unit_id = ?,
        ar_organization_unit_type_id = ?,
        ar_party_id = ?,
        ar_rate_category_id = ?,
        `exists` = ?,
        `from` = ?,
        ar_parent_organization_unit_id = ?,
        extension_name = ?,
        extension_user_code = ?,
        extension_codes = ?
        ');
        $stm->execute(array(
                $structXId++,
                $org5, // organization id
                $typeId, // type id
                null, // party
                null, // rate category
                1, // exists
                fromUnixTimestampToMySQLTimestamp(strtotime('-3 months')),
                $org2, // parent organization id
                null, // extension complete name
                '123', // extension short name
                '123' // extension codes
            )
        );


        $exportJob->exportConfigurationFiles($refTime);
        $r = RateEngineService::executeOrganizationTests($refTime, 6) && $r;


        return $r;
    }

    /**
     * @return bool true if all test are ok
     */
    protected function unitTestOfReportWorkflow()
    {
        $d = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();

        // Add one ArCdr, otherwise report scheduling is locked
        $cdr = new ArCdr();
        $cdr->setCalldate($d);
        $cdr->setId(200);
        $cdr->setDestinationType(DestinationType::ignored);
        $cdr->setExportedInternalTelephoneNumber('');
        $cdr->save();

        $c = new Criteria();
        ArNewProblemPeer::doDelete($c);

        $regressionReportId = 'REGRESSION REPORT IDENTIFIER';

        // Remove previous generated reports

        try {
            $c = new Criteria();
            $c->add(ArReportSchedulerPeer::INTERNAL_NAME, $regressionReportId);
            ArReportSchedulerPeer::doDelete($c);
        } catch (Exception $e) {

        }

        try {
            $c = new Criteria();
            $c->add(ArReportPeer::INTERNAL_NAME, $regressionReportId);
            ArReportPeer::doDelete($c);
        } catch (Exception $e) {

        }

        //
        // Test scheduling of reports
        //

        // add a CDR because otherwise reports are not generated
        $cdr = new ArCdr();
        $cdr->setCalldate($d);
        $cdr->setId(100);
        $cdr->setExportedInternalTelephoneNumber('');
        $cdr->save();

        $report = new ArReport();
        $report->setInternalName($regressionReportId);
        $report->setPhpClassName('BillingReport_UserDefined');
        $report->setReportName('Brief Institution Report');
        $report->setIsTemplate(true);
        $report->setArOrganizationUnitId(OrganizationUnitInfo::getInstance()->getUniqueRootOrganizationIdIfExists());
        $report->setParamExpandToLevel(1);
        $report->setParamShowAlsoOutgoingCalls(true);
        $report->setParamShowAlsoInternalCalls(false);
        $report->setParamShowAlsoIncomingCalls(false);
        $report->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_NAME);
        $report->setParamShowVoipProvider(false);
        $report->setParamShowCommunicationChannel(false);
        $report->setParamShowMaskedTelephoneNumbers(true);
        $report->setParamShowCallCost(false);
        $report->setParamShowCallIncome(true);
        $report->setParamShowCostSaving(false);
        $report->setParamShowCallDetails(false);
        $report->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($report->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
        $p->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($report->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $p->save();

        $reportTemplateId = $report->getId();

        $s = new ArReportScheduler();
        $s->setInternalName($regressionReportId);
        $s->setArReportId($reportTemplateId);
        $s->setIsActive(true);
        $s->setArOrganizationUnitId(OrganizationUnitInfo::getInstance()->getUniqueRootOrganizationIdIfExists());
        $s->setArReportGenerationId(ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS);
        $s->setNote('Brief per institution report.');
        $s->setProducedReportMustBeReviewed(true);
        $s->setScheduleEveryXMonths(1);
        $s->initForMonthly(1, 1);
        $s->save();

        $atTime = strtotime("-2 month");
        $atTime = startWith00Timestamp($atTime);

        // Simulate report generation

        $generator = new ScheduledReportGenerator();
        $generator->setArReportScheduler($s);

        // Start Tests

        $i = 0;
        $r = true;

        // 1
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfReportWorkflow $i",
                date('c', $generator->getReportRangeToDate($atTime)),
                date('c', strtotime('+1 month', $atTime))
            ) && $r;

        // 2
        $i++;
        $r = $this->unitTestOfCondition(
                "unitTestOfReportWorkflow $i",
                $generator->getCanBeExecuted(strtotime('+40 days', fromMySQLTimestampToUnixTimestamp($generator->getArReportScheduler()->getLastToDate()))),
                true
            ) && $r;

        //
        // Test Report Permissions
        //

        $workDate0 = strtotime('+1 month', FixedJobProcessor::getGlobalStartingDateForCDRProcessinng());
        $workDate1 = strtotime('+14 days', $workDate0);
        $workDate2 = strtotime('+14 days', $workDate1);
        $workDate3 = strtotime('+14 days', $workDate2);

        // Create organization hierarchy

        $typeId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_GENERIC_ORG)->getId();
        $normalCategoryId = ArRateCategoryPeer::retrieveByInternalName(ArRateCategory::ID_FOR_NORMAL)->getId();

        $party1 = new ArParty();
        $party1->setName('A');
        $party1->save();

        $org1 = new ArOrganizationUnit();
        $org1->save();
        $orgA = $org1;

        $struct1 = new ArOrganizationUnitHasStructure();
        $struct1->setArOrganizationUnitId($org1->getId());
        $struct1->setArOrganizationUnitTypeId($typeId);
        $struct1->setArPartyId($party1->getId());
        $struct1->setArRateCategoryId($normalCategoryId);
        $struct1->setExists(true);
        $struct1->setFrom($workDate0);
        $struct1->setArParentOrganizationUnitId(null);
        $struct1->save();


        $party1 = new ArParty();
        $party1->setName('B1');
        $party1->save();

        $org1 = new ArOrganizationUnit();
        $org1->save();
        $orgB1 = $org1;

        $struct1 = new ArOrganizationUnitHasStructure();
        $struct1->setArOrganizationUnitId($org1->getId());
        $struct1->setArOrganizationUnitTypeId($typeId);
        $struct1->setArPartyId($party1->getId());
        $struct1->setArRateCategoryId($normalCategoryId);
        $struct1->setExists(true);
        $struct1->setFrom($workDate0);
        $struct1->setArParentOrganizationUnitId($orgA->getId());
        $struct1->save();

        $party1 = new ArParty();
        $party1->setName('C1');
        $party1->save();

        $org1 = new ArOrganizationUnit();
        $org1->save();
        $orgC1 = $org1;

        $struct1 = new ArOrganizationUnitHasStructure();
        $struct1->setArOrganizationUnitId($org1->getId());
        $struct1->setArOrganizationUnitTypeId($typeId);
        $struct1->setArPartyId($party1->getId());
        $struct1->setArRateCategoryId($normalCategoryId);
        $struct1->setExists(true);
        $struct1->setFrom($workDate0);
        $struct1->setArParentOrganizationUnitId($orgB1->getId());
        $struct1->save();


        $party1 = new ArParty();
        $party1->setName('B2');
        $party1->save();

        $org1 = new ArOrganizationUnit();
        $org1->save();
        $orgB2 = $org1;

        $struct1 = new ArOrganizationUnitHasStructure();
        $struct1->setArOrganizationUnitId($org1->getId());
        $struct1->setArOrganizationUnitTypeId($typeId);
        $struct1->setArPartyId($party1->getId());
        $struct1->setArRateCategoryId($normalCategoryId);
        $struct1->setExists(true);
        $struct1->setFrom($workDate0);
        $struct1->setArParentOrganizationUnitId($orgA->getId());
        $struct1->save();


        $party1 = new ArParty();
        $party1->setName('C2');
        $party1->save();

        $org1 = new ArOrganizationUnit();
        $org1->save();
        $orgC2 = $org1;

        $struct1 = new ArOrganizationUnitHasStructure();
        $struct1->setArOrganizationUnitId($org1->getId());
        $struct1->setArOrganizationUnitTypeId($typeId);
        $struct1->setArPartyId($party1->getId());
        $struct1->setArRateCategoryId($normalCategoryId);
        $struct1->setExists(true);
        $struct1->setFrom($workDate0);
        $struct1->setArParentOrganizationUnitId($orgB2->getId());
        $struct1->save();

        // Create users

        $party = new ArParty();
        $party->setName('A');
        $party->save();

        $user = new ArUser();
        $user->setLogin('A');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($orgA->getId());
        $user->setIsEnabled(true);
        $user->save();

        $userA = $user;

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $rel->save();

        $party = new ArParty();
        $party->setName('B1');
        $party->save();

        $user = new ArUser();
        $user->setLogin('B1');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($orgB1->getId());
        $user->setIsEnabled(true);
        $user->save();

        $userB1 = $user;

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $rel->save();

        $party = new ArParty();
        $party->setName('C1');
        $party->save();

        $user = new ArUser();
        $user->setLogin('C1');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($orgC1->getId());
        $user->setIsEnabled(true);
        $user->save();

        $userC1 = $user;

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $rel->save();


        $party = new ArParty();
        $party->setName('B2');
        $party->save();

        $user = new ArUser();
        $user->setLogin('B2');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($orgB2->getId());
        $user->setIsEnabled(true);
        $user->save();

        $userB2 = $user;

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $rel->save();


        $party = new ArParty();
        $party->setName('C2');
        $party->save();

        $user = new ArUser();
        $user->setLogin('C2');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($orgC2->getId());
        $user->setIsEnabled(true);
        $user->save();

        $userC2 = $user;

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $rel->save();

        $party = new ArParty();
        $party->setName('Root');
        $party->save();

        $user = new ArUser();
        $user->setLogin('Root');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId(null);
        $user->setIsRootAdmin(true);
        $user->setIsEnabled(true);
        $user->save();

        $userAdmin = $user;

        $party = new ArParty();
        $party->setName('B1 Accountant');
        $party->save();

        $user = new ArUser();
        $user->setLogin('B1 A');
        $user->setArPartyId($party->getId());
        $user->setArOrganizationUnitId($orgB1->getId());
        $user->setIsEnabled(true);
        $user->save();

        $userB1Accountant = $user;

        $rel = new ArUserHasRole();
        $rel->setArUserId($user->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
        $rel->save();

        // Create reports

        $report = new ArReport();
        $report->setInternalName($regressionReportId);
        $report->setPhpClassName('BillingReport_UserDefined');
        $report->setReportName('Brief Institution Report');
        $report->setIsTemplate(false);
        $report->setFromDate($workDate0);
        $report->setToDate(strtotime('+1 month', $workDate0));
        $report->setArOrganizationUnitId($orgA->getId());
        $report->setParamExpandToLevel(1);
        $report->setParamShowAlsoOutgoingCalls(true);
        $report->setParamShowAlsoInternalCalls(false);
        $report->setParamShowAlsoIncomingCalls(false);
        $report->setArReportOrderOfChildrenId(ArReportOrderOfChildren::ORDER_BY_NAME);
        $report->setParamShowVoipProvider(false);
        $report->setParamShowCommunicationChannel(false);
        $report->setParamShowMaskedTelephoneNumbers(true);
        $report->setParamShowCallCost(false);
        $report->setParamShowCallIncome(true);
        $report->setParamShowCostSaving(false);
        $report->setParamShowCallDetails(false);
        $report->setProducedReportAlreadyReviewed(false);
        $report->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($report->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
        $p->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($report->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::USER)->getId());
        $p->save();

        $this->publishReport($report, false);

        // 3 - users can not view not reviewed reports
        $i++;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A",
                $userA, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - B",
                $userAdmin, $report, false
            ) && $r;

        // 4 - report became reviewed
        $i++;

        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A",
                $userA, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - B",
                $userB1Accountant, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - C",
                $userAdmin, $report, false
            ) && $r;


        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - D",
                $userB1, $report, false
            ) && $r;


        // 5
        $i++;

        $report->setArOrganizationUnitId($orgB1->getId());
        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A",
                $userA, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - B",
                $userB1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - C",
                $userB2, $report, false
            ) && $r;


        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - D",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - E",
                $userB1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - F",
                $userB1Accountant, $report, true
            ) && $r;

        // 6

        $i++;

        $report->setArOrganizationUnitId($orgC1->getId());
        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A",
                $userA, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - B",
                $userB1, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - C",
                $userC1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - D",
                $userB2, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - E",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - F",
                $userB1, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - G",
                $userB1Accountant, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - H",
                $userC1, $report, true
            ) && $r;


        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - H",
                $userC2, $report, false
            ) && $r;

        // 7 - change user permissions and roles
        $i++;

        $report->setArOrganizationUnitId($orgC2->getId());
        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 1",
                $userB1Accountant, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 2",
                $userC2, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 3",
                $userB1, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 4",
                $userB2, $report, false
            ) && $r;

        $userB1Accountant->setArOrganizationUnitId($orgB2->getId());
        $userB1Accountant->save();
        $userC1->setArOrganizationUnitId($orgC2->getId());
        $userC1->save();

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 5",
                $userB1Accountant, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 6",
                $userC2, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 7",
                $userC2, $report, true
            ) && $r;


        // 8 - add new role
        $i++;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 1",
                $userA, $report, false
            ) && $r;

        $rel = new ArUserHasRole();
        $rel->setArUserId($userA->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ACCOUNTANT)->getId());
        $rel->save();

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 2",
                $userA, $report, false
            ) && $r;

        $rel->delete();

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 3",
                $userA, $report, false
            ) && $r;

        // 9 - test "only for"

        $i++;

        $userB1Accountant->setArOrganizationUnitId($orgB1->getId());
        $userB1Accountant->save();
        $userC1->setArOrganizationUnitId($orgC1->getId());
        $userC1->save();

        $p = new ArReportAlsoFor();
        $p->setArReportId($report->getId());
        $p->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $p->save();

        $report->setArOrganizationUnitId($orgB1->getId());
        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A1",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 1",
                $userA, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 2",
                $userB1, $report, true
            ) && $r;

        $rel = new ArUserHasRole();
        $rel->setArUserId($userB1->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $rel->save();
        $rel1 = $rel;

        $rel = new ArUserHasRole();
        $rel->setArUserId($userA->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $rel->save();
        $rel2 = $rel;

        $rel = new ArUserHasRole();
        $rel->setArUserId($userAdmin->getId());
        $rel->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
        $rel->save();
        $rel3 = $rel;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A2",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 3",
                $userB1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 4",
                $userA, $report, false
            ) && $r;

        $rel1->delete();
        $rel2->delete();
        $rel3->delete();

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - A3",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 4",
                $userA, $report, false
            ) && $r;

        $p->delete();
        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 5",
                $userB1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 6",
                $userA, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 7",
                $userAdmin, $report, false
            ) && $r;

        // 10 - test users that are not enabled
        $i++;
        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 1",
                $userB1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 2",
                $userB1Accountant, $report, true
            ) && $r;


        $userB1->setIsEnabled(false);
        $userB1->save();

        $userB1Accountant->setIsEnabled(false);
        $userB1Accountant->save();

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 3",
                $userB1, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 4",
                $userB1Accountant, $report, false
            ) && $r;

        $userB1->setIsEnabled(true);
        $userB1->save();

        $userB1Accountant->setIsEnabled(true);
        $userB1Accountant->save();

        // 11 - test can view complete telephone numbers
        $i++;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 0",
                $userB1, $report, true
            ) && $r;

        $report->setParamShowMaskedTelephoneNumbers(false);
        $this->publishReport($report, true);

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 1",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 2",
                $userB1, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 3",
                $userC1, $report, false
            ) && $r;

        $rel = new ArUserHasPermission();
        $rel->setArUserId($userAdmin->getId());
        $rel->setArPermissionId(ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS);
        $rel->save();
        $rel1 = $rel;

        $rel = new ArUserHasPermission();
        $rel->setArUserId($userB1->getId());
        $rel->setArPermissionId(ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS);
        $rel->save();
        $rel2 = $rel;

        $rel = new ArUserHasPermission();
        $rel->setArUserId($userC1->getId());
        $rel->setArPermissionId(ArPermission::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS);
        $rel->save();
        $rel3 = $rel;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 4",
                $userAdmin, $report, false
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 5",
                $userB1, $report, true
            ) && $r;

        $r = $this->unitTestIfUserCanViewReport(
                "unitTestOfReportWorkflow $i - 6",
                $userC1, $report, false
            ) && $r;

        // NOTE: I do not test the change of hierarchy, because it requires new report generation

        return $r;
    }

    /**
     * Report speed tests.
     *
     * @return bool
     */
    protected function unitTestOfSpeed()
    {

        $nrOfRecords = 100000;

        $info = "\n   Test of speed: ";

        $filename = '/tmp/cdrs.csv';

        $fh = fopen($filename, 'w');
        assert($fh !== false);

        echo $info . "generate CSV file.";

        for ($i = 0; $i < $nrOfRecords; $i++) {

            $now = fromUnixTimestampToMySQLTimestamp(time());

            $line = "\"$now\",1," . DestinationType::ignored . ",0,10,10,\"abcdefgh\",0,\"some source $i\",\"some internal $i\",\"some external $i\",\"vendor domain\"";


            fwrite($fh, $line . "\n");
        }

        fclose($fh);

        echo $info . "load data into database.";

        $prof = new JobProfiler('CDRs');

        $conn = Propel::getConnection();

        $conn->beginTransaction();

        try {
            $result = $conn->exec("LOAD DATA INFILE '$filename'
    INTO TABLE ar_cdr
    FIELDS TERMINATED BY ','
    OPTIONALLY ENCLOSED BY '\"'
    LINES TERMINATED BY '\n'
    (calldate,
    count_of_calls,
    destination_type,
    is_redirect,
    call_ringing_duration,
    duration,
    billsec,
    cached_parent_id_hierarchy,
    is_exported,
    source_voip_account,
    source_internal_telephone_number,
    source_external_telephone_number,
    source_vendor_domain)
    ");

            if ($result === false) {
                $conn->rollBack();
                return false;
            }

            echo $info . 'commit transaction.';

            $result = $conn->commit();
            if ($result === false) {
                return false;
            }

        } catch (Exception $e) {
            echo $info . 'error ' . $e->getMessage();
            return false;
        }

        $prof->addToProcessedUnits($nrOfRecords);

        echo $info . $prof->stop();

        return true;
    }
}
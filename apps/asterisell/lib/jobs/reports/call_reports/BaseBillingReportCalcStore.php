<?php

/* $LICENSE 2012, 2013:
 *
 * Copyright (C) 2012, 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Contains groups and sum values useful for calculating a Billing Report.
 *
 * For understanding the code these concepts must be taken in account:
 * - an organization can change parent during the report time range,
 *   so during grouping and sums, it is used the parent hierarchy
 *   of the CDR calldate, and not of the report.
 * - for using a unique id representing an organization with its unique place
 *   in the hierarchy, it is used as id of an organization all its parents IDs.
 *   In this way "1/5/3" identifies uniquely an organization in the right
 *   hierarchy, and it is indipendent from the time.
 * - extensions associated directly to a organization, are associated
 *   to an abstract extension group, in order to classify all the calls
 *   in a hierarchy.
 * - calcs are done mainly on the SQL side.
 *
 * NOTE: there is no big differences in number of records, between CDRs grouped with all combinations,
 * or with already filtered values (destination type, vendor and so on), so a unique query is done,
 * and it can be shared between different type of reports.
 * In particular there are many fullIds, but very few variants of vendors, and destination types,
 * so they can be stored in an efficient way.
 */
class BaseBillingReportCalcStore extends ReportCalcStore
{

    /**
     * @var int|null
     */
    public $fromTime = null;

    /**
     * @var int|null
     */
    public $toTime = null;

    /**
     * @var array ids => destination_type => ar_vendor_id => ar_communication channel => (count of calls, duration, cost, income, cost savings)
     */
    public $values;

    /**
     * @var array ids => name of last part of hierarchy chain
     */
    public $idsToLastName;

    /**
     * @var array ids => array where the keys are the ids that are direct children
     */
    public $idsToChildren;

    /**
     * @var array ids => bool the ids in the key, are ids having direct calls, and not only inherited calls
     */
    public $idsWithDirectCalls;

    /**
     * @var array
     */
    public $rootIds;

    /**
     * @return array ids => bool the organization id of the root organizations, in "/id/" format
     */
    public function getRootIds()
    {
        return $this->rootIds;
    }

    /**
     * @param string $ids
     * @return bool true if $ids has some direct children
     */
    public function hasChildren($ids)
    {
        if (isset($this->idsToChildren[$ids])) {
            return (count($this->idsToChildren[$ids]) > 0);
        } else {
            return false;
        }
    }

    /**
     * @param string|null $rootIds null for all childrens
     * @return string[] the children ids of $rootIds (and $rootIds itself) having direct calls to display
     */
    public function getChildrenIdsWithDirectCalls($rootIds)
    {
        $r = array();
        foreach ($this->idsWithDirectCalls as $ids => $ignore) {
            if (is_null($rootIds)) {
                $r[] = $ids;
            } else {
                if (OrganizationUnitInfo::isChildrenUsingCachedParentIdHierarchy($rootIds, $ids)) {
                    $r[] = $ids;
                }
            }
        }

        return $r;
    }

    protected $isEmpty;

    public function isEmpty()
    {
        return $this->isEmpty;
    }

    /**
     * Return the name of the $ids, in a fast way, omitting the $rootIds part (as implicit).
     *
     * @param int $rootIdsLevel omit the first $ids element on the left, because implicits, 0 for no omissions
     * @param string|null $ids null for root level
     * @param bool $fromRootToChildOrder true for returning the name starting from root organization,
     * false for starting from children
     * @return string
     */
    public function getCompleteNameInAFastWay($rootIdsLevel, $ids, $fromRootToChildOrder)
    {

        if (is_null($ids)) {
            return ReportGenerator::getNameOfReportAssociatedToAllRootOrganizations();
        }

        $idsArr = OrganizationUnitInfo::getParentIdsFromCachedParentIdHierarchy($ids);

        $r = '';
        $sep = '';
        $fullIds = '/';
        $c = 0;
        foreach ($idsArr as $idElement) {
            $fullIds .= $idElement . '/';

            if ($c >= $rootIdsLevel) {
                if (isset($this->idsToLastName[$fullIds])) {
                    $n = $this->idsToLastName[$fullIds];

                    if ($fromRootToChildOrder) {
                        $r = $r . $sep . $n;
                    } else {
                        $r = $n . $sep . $r;
                    }
                    $sep = ' / ';
                } else {
                    // if there are reports without full data, it can happen we have no this type of info,
                    // so we generate it outside of cache. Not very elegant...

                    if (OrganizationUnitInfo::getFullIdsNestingLevel($ids) > 0) {
                        $organizationId = OrganizationUnitInfo::getLastId($ids);
                        $rootId = OrganizationUnitInfo::getIdInFullIds($ids, $rootIdsLevel);
                        $name = OrganizationUnitInfo::getInstance()->getFullNameAtDate($organizationId, $this->fromTime, false, false, $rootId, false);
                        return $this->convertString($name);
                    } else {
                        return '';
                    }
                }
            }
            $c++;
        }

        return $r;
    }

    protected $properVendors = null;

    /**
     * @return array of $vendorId => true for all vendors that are also non internal
     */
    public function getProperVendors()
    {
        return $this->properVendors;
    }

    protected $allVendors = null;

    /**
     * @return array of $vendorId => true
     */
    public function getAllVendors()
    {
        return $this->allVendors;
    }

    protected $allChannels = null;

    /**
     * @return array of $channelId => true
     */
    public function getAllChannels()
    {
        return $this->allChannels;
    }

    /**
     * Process the store
     *
     * @param int $from
     * @param int|null $to
     * @param PropelPDO $conn
     */
    public function process($from, $to, PropelPDO $conn)
    {
        $this->fromTime = $from;
        $this->toTime = $to;


        $query = 'SELECT
    ar_cdr.cached_parent_id_hierarchy AS organization_hierarchy
  , ar_cdr.destination_type AS destination_type
  , ar_cdr.ar_vendor_id AS ar_vendor_id
  , ar_cdr.ar_communication_channel_type_id AS ar_communication_channel_id
  , ar_telephone_prefix.geographic_location
  , ar_telephone_prefix.operator_type
  , SUM(ar_cdr.count_of_calls) AS count_of_calls
  , SUM(ar_cdr.income) AS income
  , SUM(ar_cdr.cost) AS cost
  , SUM(ar_cdr.cost_saving) AS cost_saving
  , SUM(ar_cdr.billsec) AS duration
  , ar_vendor.is_internal AS is_internal_vendor
FROM ar_cdr FORCE INDEX (ar_cdr_calldate_index)
JOIN ar_vendor ON ar_cdr.ar_vendor_id = ar_vendor.id
JOIN ar_telephone_prefix ON ar_cdr.ar_telephone_prefix_id = ar_telephone_prefix.id
WHERE ar_cdr.destination_type <> ' . DestinationType::error . '
AND ar_cdr.destination_type <> ' . DestinationType::ignored . '
AND ar_cdr.destination_type <> ' . DestinationType::known_error . '
AND ar_cdr.calldate >= ?
';

        $params = array();

        $params[] = fromUnixTimestampToMySQLTimestamp($this->fromTime);

        if (!isEmptyOrNull($this->toTime)) {
            $query .= ' AND ar_cdr.calldate < ?';
            $params[] = fromUnixTimestampToMySQLTimestamp($this->toTime);
        }

        $query .= 'GROUP BY
        ar_cdr.cached_parent_id_hierarchy,
        ar_cdr.destination_type,
        ar_cdr.ar_vendor_id,
        ar_cdr.ar_communication_channel_type_id,
        ar_telephone_prefix.geographic_location,
        ar_telephone_prefix.operator_type
        ';

        // Scan all CDRs in the date range, completing stats

        $hierarchy = OrganizationUnitInfo::getInstance();

        $this->isEmpty = true;
        $this->values = array();
        $this->idsToChildren = array();
        $this->idsToLastName = array();
        $this->idsWithDirectCalls = array();
        $this->properVendors = array();
        $this->allVendors = array();
        $this->allChannels = array();
        $this->rootIds = array();

        $stm = $conn->prepare($query);
        $stm->execute($params);
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {

            $this->isEmpty = false;

            $i = 0;
            $organizationIds = $rs[$i++];
            $direction = $rs[$i++];
            $vendorId = $rs[$i++];
            $channelId = $rs[$i++];
            $geographicLocation = $rs[$i++];
            $operatorType = $rs[$i++];

            $countOfCalls = $rs[$i++];
            $income = $rs[$i++];
            $cost = $rs[$i++];
            $saving = $rs[$i++];
            $duration = $rs[$i++];
            $isInternalVendor = $rs[$i++];

            $this->idsWithDirectCalls[$organizationIds] = true;
            $this->allVendors[$vendorId] = true;
            $this->allChannels[$channelId] = true;
            if (!$isInternalVendor) {
                $this->properVendors[$vendorId] = true;
            }

            // Add the values to each parent, because each parent inherits the direct values of the children.
            // The special parent '/' identifies the root of all root nodes.
            $ids = '/';
            $idsLen = 0;
            $firstSuperRoot = true;
            $prevParent = '';
            foreach (explode('/', $organizationIds) as $idElement) {

                $process = true;
                if (!isEmptyOrNull($idElement)) {
                    $prevParent = $ids;
                    $ids .= $idElement . '/';

                    $idsLen++;
                    if ($idsLen == 1) {
                        $this->rootIds[$ids] = true;
                    }
                } else if ($firstSuperRoot) {
                    // add values to super-root '/'
                } else {
                    $process = false;
                }

                if ($process) {
                    if (isset($this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation][$operatorType])) {
                        list($countOfCalls1, $duration1, $cost1, $income1, $saving1) = $this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation][$operatorType];
                        $this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation][$operatorType] = array($countOfCalls + $countOfCalls1, $duration + $duration1, $cost + $cost1, $income + $income1, $saving + $saving1);
                    } else {

                        if (!isset($this->values[$ids])) {
                            $this->values[$ids] = array();
                        } else if (!isset($this->values[$ids][$direction])) {
                            $this->values[$ids][$direction] = array();
                        } else if (!isset($this->values[$ids][$direction][$vendorId])) {
                            $this->values[$ids][$direction][$vendorId] = array();
                        } else if (!isset($this->values[$ids][$direction][$vendorId][$channelId])) {
                            $this->values[$ids][$direction][$vendorId][$channelId] = array();
                        } else if (!isset($this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation])) {
                            $this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation] = array();
                        }

                        // this is the condition of the main if test case
                        assert(!isset($this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation][$operatorType]));

                        $this->values[$ids][$direction][$vendorId][$channelId][$geographicLocation][$operatorType] = array($countOfCalls, $duration, $cost, $income, $saving);
                    }

                    if (!$firstSuperRoot) {
                        // Precompute names, and direct children, for speeding up report generation
                        if (!isset($this->idsToLastName[$ids])) {

                            // NOTE: an organization must exist in the timeframe of the report.
                            // The best result is returning the name at the moment of the CDR analyzed, because
                            // an organization can change name overtime.
                            // In practice return the name of the organization at the moment of the first CDR of the report,
                            // or the first name in the future (according the API or hierarchy info), if the organization
                            // is created later in report life.

                            // NOTE: in any case it recognize if an organization change hierarchy in the report timeframe,
                            // because it has a different $parentIds

                            // NOTE: if you change the way names are generated, then similar changes must be applied
                            // to code in `getCompleteNameInAFastWay(...)`

                            $this->idsToLastName[$ids] = $hierarchy->getShortName($idElement, $this->fromTime, false, false);

                            if (!isset($this->idsToChildren[$prevParent])) {
                                $this->idsToChildren[$prevParent] = array();
                            }
                            $this->idsToChildren[$prevParent][$ids] = true;
                        }
                    }
                    $firstSuperRoot = false;
                }
            }
        }

        $stm->closeCursor();

    }

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

            if(! is_null($destinationType)) {
                if(! is_null($vendorId)) {
                    if(! is_null($communicationChannelId)) {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }

                    } else {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }
                    }

                } else {
                    if(! is_null($communicationChannelId)) {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }

                    } else {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                if (isset($arr0[$destinationType])) {
                                    $arr1 = $arr0[$destinationType];
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }
                    }
                }

            } else {
                if(! is_null($vendorId)) {
                    if(! is_null($communicationChannelId)) {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }

                    } else {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    if (isset($arr1[$vendorId])) {
                                        $arr2 = $arr1[$vendorId];
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }
                    }

                } else {
                    if(! is_null($communicationChannelId)) {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        if (isset($arr2[$communicationChannelId])) {
                                            $arr3 = $arr2[$communicationChannelId];
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }

                    } else {
                        if(! is_null($geographicLocation)) {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            if (isset($arr3[$geographicLocation])) {
                                                $arr4 = $arr3[$geographicLocation];
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }

                        } else {
                            if(! is_null($operatorType)) {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                if (isset($arr4[$operatorType])) {
                                                    $arr5 = $arr4[$operatorType];
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);

                            } else {

                                $arr0 = $this->values[$ids];
                                foreach($arr0 as $arr1) {
                                    foreach($arr1 as $arr2) {
                                        foreach($arr2 as $arr3) {
                                            foreach($arr3 as $arr4) {
                                                foreach($arr4 as $arr5) {
                                                    list($totCount1, $totDuration1, $totCost1, $totIncome1, $totSavings1) = $arr5;
                                                    $totCount += $totCount1;
                                                    $totDuration += $totDuration1;
                                                    $totCost += $totCost1;
                                                    $totIncome += $totIncome1;
                                                    $totSavings += $totSavings1;

                                                }
                                            }
                                        }
                                    }
                                }
                                return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
                            }
                        }
                    }
                }
            }
        } else {
            return array($totCount, $totDuration, $totCost, $totIncome, $totSavings);
        }
   }
}

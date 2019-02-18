<?php
// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

/**
 * Info about an OrganizationUnit in a specific date.
 *
 * This class contains all the domain logic for managing hierarchies of organizations.
 *
 * It has these purpose:
 *   - converting complex database data into PHP friendly data structures;
 *   - putting in the same place all the logic for accessing hierarchy data;
 *   - caching the data, for fast access during processing;
 *
 * This code assumes that an organization hierarchy:
 *   - can be easily cached in an array
 *   - it does not change too much during time (it does not change every day...),
 *     so there are not many records storing changes.
 *
 * All the info, of all the hierarchies, is put in a cached array.
 * Advantages:
 * - only a single query is called
 * - no intermediate objects are created but only arrays
 *
 * I don't group by root node, because:
 * - code is simpler
 * - in any case the admin must know all the info
 * - there are not so many concurrent users for this optimization
 * - I have no problems in case of infinite loops
 *
 * In case of many concurrent users, then a distinct array with only relevant data, can be useful.
 *
 * CONTRACTS FOR DEVELOPERS WHEN MODIFY THIS CLASS:
 *   - classes like `ArOrganizationUnitHasStructure` cached from this class,
 *   invalidate the cache after a save (see the save method as example)
 */
class OrganizationUnitInfo
{
    ////////////////////////
    // SINGLETONE SUPPORT //
    ////////////////////////

    static protected $instance = null;

    /**
     * Singletone instance.
     *
     * @return OrganizationUnitInfo
     */
    static public function getInstance()
    {
        if (is_null(self::$instance)) {

            // Check if there is an alreaday materiazialiezd view in the database.

            $code = 'OrganizationUnitInfo';

            $conn = Propel::getConnection();
            $query = "SELECT content FROM ar_cached_organization_info WHERE internal_name = ?";
            $stm = $conn->prepare($query);
            $stm->execute(array($code));
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                try {
                    self::$instance = unserialize($rs[0]);

                    if (self::$instance === FALSE) {
                        self::$instance = null;
                    } else if (!(self::$instance instanceof OrganizationUnitInfo)) {
                        self::$instance = null;
                    }

                } catch (Exception $e) {
                    self::$instance = null;
                }
            }
            $stm->closeCursor();

            // Calculate the value, and store in the cache

            if (is_null(self::$instance)) {
                self::$instance = new OrganizationUnitInfo();
                self::$instance->initInternalData();

                $conn = Propel::getConnection();
                $query = "INSERT INTO ar_cached_organization_info(internal_name, content) VALUES(?, ?);";
                $stm = $conn->prepare($query);
                $stm->execute(array($code, serialize(self::$instance)));
                $stm->closeCursor();
            }
        }

        return self::$instance;
    }

    static protected $disableResetCachedValues = false;

    static public function disableResetCachedValues()
    {
        self::$disableResetCachedValues = true;
    }

    static public function enableResetCachedValues()
    {
        self::$disableResetCachedValues = false;
    }

    /**
     * Call for resetting the cache, after loading new data,
     * and forcing a reloading of new info.
     *
     * @static
     *
     */
    static public function resetCachedValues()
    {
        if (!self::$disableResetCachedValues) {
            self::$instance = null;
        }
    }

    //////////////////////////////
    // INTERNAL DATA MANAGEMENT //
    //////////////////////////////


    const DATA_UNIT_ID = 0;
    const DATA_NOT_USED_1 = 1;
    const DATA_UNIT_IS_BILLABLE = 2;
    const DATA_UNIT_INTERNAL_NAME = 3;
    const DATA_UNIT_INTERNAL_NAME2 = 4;
    const DATA_UNIT_TYPE_ID = 5;
    const DATA_UNIT_TYPE_NAME = 6;
    const DATA_UNIT_TYPE_SHORT_CODE = 7;
    const DATA_UNIT_TYPE_INTERNAL_NAME = 8;
    const DATA_UNIT_TYPE_IS_LEAF = 9;
    const DATA_STRUCTURE_ID = 10;
    const DATA_AR_ORGANIZATION_UNIT_ID = 11;
    const DATA_STRUCTURE_AR_PARENT_ORGANIZATION_UNIT_ID = 12;
    const DATA_STRUCTURE_FROM = 13;
    const DATA_STRUCTURE_EXISTS = 14;
    const DATA_STRUCTURE_AR_RATE_CATEGORY_ID = 15;
    const DATA_PARTY_ID = 16;
    const DATA_PARTY_NAME = 17;
    const DATA_PARTY_COMPACT_NAME = 18;
    const DATA_NOT_USED_19 = 19;
    const DATA_NOT_USED_20 = 20;
    const DATA_NOT_USED_21 = 21;
    const DATA_NOT_USED_22 = 22;
    const DATA_ORGANIZATION_MANAGED_FROM = 23;
    const DATA_EXTENSION_CODES = 24;
    const DATA_EXTENSION_NAME = 25;
    const DATA_EXTENSION_USER_CODE = 26;
    const DATA_EXPORT_CODE = 27;
    const DATA_PARTY_CRM = 28;
    const DATA_PARTY_NOTE = 29;

    /**
     * The info associated to an organization, in descending order by date.
     *
     * > organizationId => array data-info-value
     *
     * @var array data in DATA_... format
     */
    public $idToStructureData;

    /**
     * The direct children, with their fromDate, in descending order by date.
     * Something like
     *
     * > parentId => array(childrenId, fromDate);
     *
     * @var array
     */
    public $idToDirectChildrenIds;

    /**
     * @var array organizationId => userId => array of (ar_permission.id)
     */
    protected $idToUserPermissions;

    /**
     * @var array organizationId => userId => array of (ar_role.id)
     */
    protected $idToUserRoles;

    /**
     * @var array userId => string
     */
    protected $userIdToName;

    /**
     * @var array $organizationIdsThatCanBeBillable organizationId => true for organization id that can be billable at a certain date
     */
    public $organizationIdsThatCanBeBillable;

    /**
     * @var array organizationId => true for organizations that can be root organizations, now or in the past
     */
    protected $maybeRootOrganizations;

    /**
     * @var ArNewProblem[]
     */
    protected $detectedProblems;

    /**
     * @return ArNewProblem[]
     */
    public function getDetectedProblems()
    {
        if (is_null($this->detectedProblems)) {
            return array();
        } else {
            return $this->detectedProblems;
        }
    }

    /**
     * Init internal data information.
     *
     */
    protected function initInternalData()
    {

        $this->idToDirectChildrenIds = array();
        $this->idToStructureData = array();
        $this->maybeRootOrganizations = array();
        $this->detectedProblems = array();
        $this->organizationIdsThatCanBeBillable = array();

        $this->resetLastAccessedData();

        /**
         * @var array organizationId => last date of assigned extension code
         */
        $fromOrganizationIdToLastExtensionDate = array();

        // NOTE: the order of fields, must be the same of DATA_... constants
        $query = <<<SQL

SELECT
u.id AS field_0,
NULL AS field_1,
p.is_billable AS field_2,
u.internal_name AS field_3,
u.internal_name2 AS field_4,
t.id AS field_5,
t.name AS field_6,
t.short_code AS field_7,
t.internal_name AS field_8,
(s.extension_codes IS NOT NULL OR LENGTH(s.extension_codes) > 0) AS field_9,
s.id AS field_10,
s.ar_organization_unit_id AS field_11,
s.ar_parent_organization_unit_id AS field_12,
s.from AS field_13,
s.exists AS field_14,
s.ar_rate_category_id AS field_15,
p.id AS field_16,
p.name AS field_17,
p.compact_name AS field_18,
NULL AS field_19,
NULL AS field_20,
NULL AS field_21,
NULL AS field_22,
u.automatically_managed_from AS field_23,
s.extension_codes AS field_24,
s.extension_name AS field_25,
s.extension_user_code AS field_26,
u.export_code AS field_27,
p.external_crm_code AS field_28,
p.note AS field_29
FROM ar_organization_unit_has_structure AS s
     JOIN ar_organization_unit AS u ON (s.ar_organization_unit_id = u.id)
     LEFT JOIN ar_organization_unit_type AS t ON (s.ar_organization_unit_type_id = t.id)
     LEFT JOIN ar_party AS p ON (s.ar_party_id = p.id)
ORDER BY s.from DESC
;

SQL;

        $conn = Propel::getConnection();
        $stm = $conn->prepare($query);
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {

            $allExtensionCodes = $rs[self::DATA_EXTENSION_CODES];

            // Start processing

            $organizationId = $rs[self::DATA_AR_ORGANIZATION_UNIT_ID];

            $rs[self::DATA_STRUCTURE_FROM] = fromMySQLTimestampToUnixTimestamp($rs[self::DATA_STRUCTURE_FROM]);

            if (array_key_exists($organizationId, $this->idToStructureData)) {
                $this->idToStructureData[$organizationId][] = $rs;
            } else {
                $this->idToStructureData[$organizationId] = array($rs);
            }

            $childrenData = array($organizationId, $rs[self::DATA_STRUCTURE_FROM]);

            $parentId = $rs[self::DATA_STRUCTURE_AR_PARENT_ORGANIZATION_UNIT_ID];

            if (!is_null($parentId)) {
                if (array_key_exists($parentId, $this->idToDirectChildrenIds)) {
                    $this->idToDirectChildrenIds[$parentId][] = $childrenData;
                } else {
                    $this->idToDirectChildrenIds[$parentId] = array($childrenData);
                }
            } else {
                $this->maybeRootOrganizations[$organizationId] = true;
            }

            if ($rs[self::DATA_UNIT_IS_BILLABLE] && $rs[self::DATA_STRUCTURE_EXISTS]) {
                $this->organizationIdsThatCanBeBillable[intval($organizationId)] = true;
            }

            // all the next settings about this organization, stop at the date of this last setting,
            // because we are retrieving changes in descending order of date (first the most recent)
            $fromOrganizationIdToLastExtensionDate[$organizationId] = $rs[self::DATA_STRUCTURE_FROM];
        }
        $stm->closeCursor();

        //
        // Retrieve information about users and permissions
        //

        $this->idToUserPermissions = array();
        $this->idToUserRoles = array();
        $this->userIdToName = array();

        $query = 'SELECT ar_organization_unit_id, ar_user_id, ar_role_id, ar_user.login, ar_party.id
                  FROM ar_user_has_role
                  INNER JOIN ar_user ON ar_user_has_role.ar_user_id = ar_user.id
                  LEFT JOIN ar_party ON ar_user.ar_party_id = ar_party.id
                  ';
        $stm = $conn->prepare($query);
        $stm->execute();
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $organizationId = $rs[0];
            $userId = $rs[1];
            $permissionId = $rs[2];

            if (!array_key_exists($userId, $this->userIdToName)) {
                $userLogin = $rs[3];
                $partyId = $rs[4];

            }

            if (!is_null($permissionId)) {
                if (!isset($this->idToUserRoles[$organizationId])) {
                    $this->idToUserRoles[$organizationId] = array();
                }

                if (!isset($this->idToUserRoles[$organizationId][$userId])) {
                    $this->idToUserRoles[$organizationId][$userId] = array();
                }
                $this->idToUserRoles[$organizationId][$userId][] = $permissionId;
            }
        }
        $stm->closeCursor();

        $query = 'SELECT ar_organization_unit_id, ar_user_id, ar_permission_id, ar_user.login, ar_party.name, ar_party.id
                  FROM ar_user_has_permission
                  INNER JOIN ar_user ON ar_user_has_permission.ar_user_id = ar_user.id
                  LEFT JOIN ar_party ON ar_user.ar_party_id = ar_party.id
                  ';
        $stm = $conn->prepare($query);
        $stm->execute();
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $organizationId = $rs[0];
            $userId = $rs[1];
            $permissionId = $rs[2];

            if (!is_null($permissionId)) {
                if (!isset($this->idToUserPermissions[$organizationId])) {
                    $this->idToUserPermissions[$organizationId] = array();
                }

                if (!isset($this->idToUserPermissions[$organizationId][$userId])) {
                    $this->idToUserPermissions[$organizationId][$userId] = array();
                }
                $this->idToUserPermissions[$organizationId][$userId][] = $permissionId;
            }
        }
        $stm->closeCursor();
    }

    /////////////////////
    // DATA ACCESS API //
    /////////////////////

    /**
     * @var int|null
     */
    protected $lastAccessedId;

    /**
     * @var int|null
     */
    protected $lastAccessedDate;

    /**
     * @var int|null
     */
    protected $lastAccessedResult;


    protected function resetLastAccessedData()
    {
        $this->lastAccessedId = null;
        $this->lastAccessedDate = null;
        $this->lastAccessedResult = null;
    }

    /**
     * @param int $organizationId
     * @param int|null $queryDate
     * @return array|null null if the $organizationId is not known,
     *         a value with self::DATA_STRUCTURE_EXISTS to false if the organization does not exists
     *         at the specified date, but it is created in the future.
     *         The array is the array with the info data.
     *
     */
    public function getDataInfo($organizationId, $queryDate)
    {
        // cache last acessed data

        if (!is_null($this->lastAccessedId)) {
            if (($organizationId == $this->lastAccessedId) && ($queryDate == $this->lastAccessedDate)) {
                return $this->lastAccessedResult;
            }
        }

        // calculate the result

        if (is_null($queryDate)) {
            $date = time();
        } else {
            $date = $queryDate;
        }

        $result = null;
        $resultInTheFuture = null;

        if (array_key_exists($organizationId, $this->idToStructureData)) {

            foreach ($this->idToStructureData[$organizationId] as $r) {
                if ($r[self::DATA_STRUCTURE_FROM] <= $date) {
                    $result = $r;
                    $resultInTheFuture = null;

                    // NOTE: it can return the first result because the cache is ordered about from date,
                    // so it is the most recent result
                    break;
                } else {
                    // The ID exists, but the definition is in a date in the future
                    // respect $date. Copy existential fields than are always true,
                    // indipendetly from other time-related definitions.

                    // NOTE: select always the info that is nearest to $date

                    $resultInTheFuture = $r;
                }
            }

            if (!is_null($resultInTheFuture)) {
                $result = $resultInTheFuture;
                $result[self::DATA_STRUCTURE_EXISTS] = false;
            }
        }

        $this->lastAccessedId = $organizationId;
        $this->lastAccessedDate = $queryDate;
        $this->lastAccessedResult = $result;

        return $result;
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int|null
     */
    public function getMostSpecificOrganizationId($organizationId, $date = null)
    {
        $info = $this->getDataInfo($organizationId, $date);
        if ((!is_null($info)) && $info[self::DATA_STRUCTURE_EXISTS]) {
            return $info[self::DATA_AR_ORGANIZATION_UNIT_ID];
        } else {
            return null;
        }
    }

    /**
     * @return array values are all the organizationIds of past, present, and future
     */
    public function getOrganizationIds() {
        return array_keys($this->idToStructureData);
    }

    /**
     * Search (slowly) for an organization with a certain CRM.
     * @param string $crm
     * @return int|null null if it does not exist an organization with the specified CRM,
     * -2, -3 ... if exists 2 or more organizations with the same CRM
     */
    public function getOrganizationIdByCRM($crm)
    {
        $r = array();
        foreach($this->idToStructureData as $organizationId => $data) {
            foreach($data as $info) {
              if (strcmp($crm, $info[self::DATA_PARTY_CRM]) == 0) {
                $r[] = $organizationId;
              }
            }
        }

        if (count($r) == 0) {
            return null;
        } else if (count($r) > 1) {
            return (- count($r));
        } else {
            return reset($r);
        }
    }

    //////////////////
    // Basic Access //
    //////////////////

    /**
     * @param int $organizationId
     * @param int|null $date
     *
     * @return bool FALSE if the unit does not exist at this date.
     *
     * NOTE: a organization that does not exists have no upper hiearchy, and have no children,
     * because it is no part of any organization hierarchy.
     *
     * NOTE: a organization that does not exists, can have in any case a party or extension associated,
     * because these are part of the definition of the organization unit, and they never change over time.
     *
     * NOTE: exists is used for modelling organization unit that stop to exists. This info must be explicitely added
     * in the database schema, and it is returnend a non existance data starting from a certain date.
     */
    public function getExists($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        if (is_null($info)) {
            return false;
        } else {
            return $info[self::DATA_STRUCTURE_EXISTS];
        }
    }

    /**
     * NULL if it is not a party
     *
     * @param int $organizationId
     * @param int|null $date
     * @return int|null
     */
    public function getArPartyId($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_PARTY_ID];
    }

    /**
     * NULL if it is not a party
     *
     * @param int $organizationId
     * @param int|null $date
     * @return string|null the external CRM code
     */
    public function getPartyCRM($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_PARTY_CRM];
    }

    /**
     * NULL if it is not a party
     *
     * @param int $organizationId
     * @param int|null $date
     * @return string|null the note
     */
    public function getPartyNote($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_PARTY_NOTE];
    }

    /**
     * unixtimestamp from when this info is valid
     *
     * @param int $organizationId
     * @param int|null $date
     * @return int
     */
    public function getFromDate($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_STRUCTURE_FROM];
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int|null
     */
    public function getParentId($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        if (is_null($info)) {
            return null;
        } else {
            return $info[self::DATA_STRUCTURE_AR_PARENT_ORGANIZATION_UNIT_ID];
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int|null
     */
    public function getTypeId($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        if (is_null($info)) {
            return null;
        } else {
            return $info[self::DATA_UNIT_TYPE_ID];
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return string
     */
    public function getTypeName($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_UNIT_TYPE_NAME];
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return string
     */
    public function getTypeShortCode($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_UNIT_TYPE_SHORT_CODE];
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return bool
     */
    public function getTypeIsLeaf($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_UNIT_TYPE_IS_LEAF];
    }

    /**
     * The used ar_organization_unit_has_strutcture.id
     *
     * @param int $organizationId
     * @param int|null $date
     * @return int
     */
    public function getStructureId($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_STRUCTURE_ID];
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int|null
     */
    public function getArRateCategoryId($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_STRUCTURE_AR_RATE_CATEGORY_ID];
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return bool true if it is an extension
     */
    public function isExtension($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return $info[self::DATA_UNIT_TYPE_IS_LEAF];
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return bool true if it is a party
     */
    public function isParty($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);
        return !is_null($info[self::DATA_PARTY_ID]);
    }

    /**
     * @param $organizationId
     * @return int how many alternative definitions of an organization there are in the past.
     * Usually 1 for an organization with only one definition, without past history changes.
     */
    public function howManyDefinitionsInThePast($organizationId)
    {
        if (array_key_exists($organizationId, $this->idToStructureData)) {
            return count($this->idToStructureData[$organizationId]);
        } else {
            return 0;
        }
    }

    /**
     * From when this info is valid.
     *
     * @param int $organizationId
     * @param int|null $date
     * @return int
     */
    public function fromDate($organizationId, $date)
    {
        return $this->getFromDate($organizationId, $date);
    }

    /**
     * When (exclusive) the info is not valid.
     * null if the info is always valid in the future.
     *
     * @param int $organizationId
     * @param int|null $date
     * @return int|null
     */
    public function toDate($organizationId, $date)
    {

        if (is_null($date)) {
            return null;
        }

        if (array_key_exists($organizationId, $this->idToStructureData)) {

            // NOTE: info is in descending order on date, so I must retrieve the last value,
            // than is the nearest value before something change.
            $lastResult = null;

            $thisId = $this->getStructureId($organizationId, $date);

            foreach ($this->idToStructureData[$organizationId] as $v) {
                $vFrom = $v[self::DATA_STRUCTURE_FROM];
                if (!is_null($vFrom)) {
                    if ($vFrom >= $date && $v[self::DATA_STRUCTURE_ID] !== $thisId) {
                        $lastResult = $vFrom;
                    }
                }
            }

            return $lastResult;
        } else {
            return null;
        }
    }

    /**
     * @param int $organizationId
     * @param int $parentId ar_organization_unit.id
     * @param int|null $date unix timestamp
     * @return bool true if $this is direct or indirect children of $parendId
     */
    public function isChildrenOfAtDate($organizationId, $date, $parentId)
    {
        if ($this->getChildrenLevelAtDate($organizationId, $date, $parentId) > 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param int $organizationId the children
     * @param int|null $date unix timestamp
     * @param int $parentId ar_organization_unit.id
     * @return int 0 if it is not a children,
     *             1 if it is direct children,
     *             2 if it is children of children,
     *             and so on...
     *             NOTE: not recognize when they are the same unit-info
     */
    public function getChildrenLevelAtDate($organizationId, $date, $parentId)
    {

        if (!$this->getExists($organizationId, $date)) {
            return 0;
        }

        $level = 0;

        $currentId = $this->getParentId($organizationId, $date);
        $avoidLoop = array();
        while (!is_null($currentId)) {
            $avoidLoop[$currentId] = true;
            $level++;

            if ($currentId == $parentId) {
                return $level;
            }

            $childId = $currentId;
            $currentId = $this->getParentId($currentId, $date);
            if (array_key_exists($currentId, $avoidLoop)) {
                $this->signalErrorForOrganizationWithInfiniteLoop($childId);
            }
        }

        return 0;
    }

    /**
     * @param int|null $permissionOnOrganizationId the organization for which an user has permissions
     * @param int|null $callsOfOrganizationId the calls to view are associated to this organization
     * @return bool true if the calls can be view, according permissions
     */
    public function canViewCallsOfOrganization($permissionOnOrganizationId, $callsOfOrganizationId)
    {
        if (isEmptyOrNull($permissionOnOrganizationId)) {
            // root admin

            return true;
        } else {
            if (isEmptyOrNull($callsOfOrganizationId)) {
                return false;
            } else {
                return ($permissionOnOrganizationId == $callsOfOrganizationId ||
                    OrganizationUnitInfo::getInstance()->isChildrenOfAtDate($callsOfOrganizationId, null, $permissionOnOrganizationId));
            }
        }
    }

    /**
     * @param string $cachedParentIds
     * @param int|null $date
     * @return int|null
     */
    public function getBillableArOrganizationId($cachedParentIds, $date)
    {
        $ids = self::getParentIdsFromCachedParentIdHierarchy($cachedParentIds);

        // NOTE: work assuming that there are few billable ids

        $r = null;
        foreach ($ids as $id) {
            $id = $id;
            if (isset($this->organizationIdsThatCanBeBillable[$id])) {
                $info = $this->getDataInfo($id, $date);
                if ($info[self::DATA_UNIT_IS_BILLABLE]) {
                    $r = $id;
                }
            }
        }

        return $r;
    }

    //////////////////
    // Descriptions //
    //////////////////

    /**
     * @param int $organizationId
     * @param int|null $date
     * @param bool $asHtml TRUE if the result is converted by htmlentities
     * @param bool $forCustomer true for generating a translate message
     * @return string a short description of the associated content.
     *
     * NOTE: this in part of the definition of the organization unit,
     * and it has a value, also if the organization does not exists.
     */
    public function getContentDescription($organizationId, $date, $asHtml = FALSE, $forCustomer = false)
    {
        $info = $this->getDataInfo($organizationId, $date);

        if (!is_null($info[self::DATA_PARTY_ID])) {
            $r = mytr('Party: ', $forCustomer) . $info[self::DATA_PARTY_NAME];
        } else {
            $r = mytr('Extension: ', $forCustomer) . $info[self::DATA_EXTENSION_NAME];
        }

        if ($asHtml) {
            return htmlspecialchars($r, ENT_QUOTES, 'UTF-8');
        } else {
            return $r;
        }
    }

    /**
     * @param array $info
     * @return bool true if the extension has an empty name
     */
    public function isEmptyExtensionName($info)
    {
        if (!is_null($info[self::DATA_PARTY_ID])) {
            return false;
        } else {
            return isEmptyOrNull($info[self::DATA_EXTENSION_NAME]);
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date unixtimestamp of the point in time wher link the unit
     * @param bool $withHtmlLink TRUE for inserting a link to the unit viewer,
     *        in this case the name is `applied to htmlentities`
     * @param bool $withTypeAnnotation true for displaying also the type of organization
     * @param bool $useOnlyExtensionShortCode true for using only the extension short code, in case of extensions
     * @return string something like "project: Apollo 13" where "project" is the name of the type.
     */
    public function getShortName($organizationId, $date = null, $withHtmlLink = FALSE, $withTypeAnnotation = TRUE, $useOnlyExtensionShortCode = false)
    {
        static $showExtensionCode = null;

        if (is_null($showExtensionCode)) {
            $showExtensionCode = sfConfig::get('app_show_extension_code');
        }

        $info = $this->getDataInfo($organizationId, $date);

        $name = '';
        if ($info[self::DATA_STRUCTURE_EXISTS] && $withTypeAnnotation) {
            // the type of the organization can be derived only for existing organizations

            $name = $info[self::DATA_UNIT_TYPE_SHORT_CODE];
            if (!isEmptyOrNull($name)) {
                $name .= ": ";
            }
        }

        if (!is_null($info[self::DATA_PARTY_ID])) {
            $name .= $info[self::DATA_PARTY_NAME];
        } else if (!isEmptyOrNull($info[self::DATA_EXTENSION_NAME])) {
            if ($useOnlyExtensionShortCode) {
                $c = $info[self::DATA_EXTENSION_USER_CODE];
                if (isEmptyOrNull($c)) {
                    $c = '';
                }
                $name .= $c;
            } else {
                $name .= $info[self::DATA_EXTENSION_NAME];
            }
            if ($showExtensionCode) {
                $extensionCode = $info[self::DATA_EXTENSION_USER_CODE];
                if ((!$useOnlyExtensionShortCode) && (!isEmptyOrNull($extensionCode)) && (strcmp($info[self::DATA_EXTENSION_NAME], $extensionCode) != 0)) {
                    $name .= ' (' . $extensionCode . ')';
                }
            }
        } else if (!isEmptyOrNull($info[self::DATA_EXTENSION_CODES])) {
            $name .= $info[self::DATA_EXTENSION_CODES];
        } else {
            $name .= 'id-' . $info[self::DATA_UNIT_ID];
        }

        if ($withHtmlLink) {
            return $this->getUrlLinkToViewer($organizationId, $date, $name, true);
        } else {
            return $name;
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return string
     */
    public function getHumanReadableName($organizationId, $date = null)
    {
        return $this->getShortName($organizationId, $date, false, false);
    }

    /**
     * @param int $organizationId
     * @param int|null $date unixtimestamp of the point in time wher link the unit
     * @return string something like "project: Apollo 13" where "project" is the name of the type,
     * and a link to the corresponding entity.
     */
    public function getShortNameWithLinkToAssociatedEntity($organizationId, $date = NULL)
    {
        $info = $this->getDataInfo($organizationId, $date);

        $name = htmlspecialchars($this->getShortName($organizationId, $date, false, true), ENT_QUOTES, 'UTF-8');

        if (!is_null($info[self::DATA_PARTY_ID])) {
            $r = 'party/edit?id=' . $info[self::DATA_PARTY_ID];
            return '<a href="' . url_for($r) . '">' . $name . '</a>';
        } else {
            return $name;
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return string|null a link to the associated entity
     */
    public function getLinkToAssociatedEntity($organizationId, $date = NULL)
    {
        $info = $this->getDataInfo($organizationId, $date);

        if (!is_null($info[self::DATA_PARTY_ID])) {
            $r = 'party/edit?id=' . $info[self::DATA_PARTY_ID];
            return url_for($r);
        } else {
            return null;
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date null for current date
     * @param bool $withHtmlLink true for inserting a link to the unit viewer.
     * In this case the result is also HTML-ESCAPED
     * @param bool $showLinkableHierarchy true for showing distinct parts of the parent hierachy as linkable,
     * false for showing a unique link, in case $withHtmlLink is true
     * @param int|null $startFromOrganizationId null for showing all the hierarchy,
     * the organization id that must be not displayed because it is implicit.
     * For example if we have "a/b/c/d", we can show only "c/d" if "a/b" is implicit.
     * @param bool $reverseOrder true for showing "d/c/b/a" in reverse order, from the most specific organization,
     * to the parent organization.
     * @param bool $hideEmptyExtensions
     * @return string something like "company: NASA / project: Apollo 13", with the entire scope listed.
     *
     * @pre $showLinkableHierarchy imply $withHtmlLink
     * NOTE: I can not reuse `$this->fromDate`, because parent hierarchies can have different changes in time,
     * that do not depend from `$this->fromDate`.
     */
    public
    function getFullNameAtDate($organizationId, $date, $withHtmlLink, $showLinkableHierarchy, $startFromOrganizationId = null, $reverseOrder = false, $hideEmptyExtensions = false)
    {
        assert(!($showLinkableHierarchy && !$withHtmlLink));

        $avoidLoop = array();

        $info = $this->getDataInfo($organizationId, $date);

        $sep = ' / ';
        $useHtmlLinkForParent = $showLinkableHierarchy;

        if ($useHtmlLinkForParent) {
            $sep = htmlspecialchars($sep, ENT_QUOTES, 'UTF-8');
        }

        if (!is_null($info) && $info[self::DATA_STRUCTURE_EXISTS] && $info[self::DATA_AR_ORGANIZATION_UNIT_ID] != $startFromOrganizationId) {

            $r = '';
            $first = true;
            $parentId = $organizationId;

            while (!is_null($info) && $info[self::DATA_STRUCTURE_EXISTS] && $parentId != $startFromOrganizationId) {
                $avoidLoop[$parentId] = true;
                if (!($hideEmptyExtensions && $this->isEmptyExtensionName($info))) {
                    $parentName = $this->getShortName($parentId, $date, $useHtmlLinkForParent);

                    if ($first) {
                        $r = $parentName;
                        $first = false;
                    } else {
                        if ($reverseOrder) {
                            $r = $r . $sep . $parentName;
                        } else {
                            $r = $parentName . $sep . $r;
                        }
                    }
                }
                $childId = $parentId;
                $parentId = $this->getParentId($parentId, $date);
                if (is_null($parentId)) {
                    $info = null;
                } else {
                    if (array_key_exists($parentId, $avoidLoop)) {
                        $this->signalErrorForOrganizationWithInfiniteLoop($childId);
                        $info = null;
                        $parentId = null;
                    } else {
                        $info = $this->getDataInfo($parentId, $date);
                    }
                }
            }

            if ($first && $hideEmptyExtensions) {
                // in case of empty extension, show the name of its parent
                $parentId = $this->getParentId($organizationId, $date);
                if (!is_null($parentId)) {
                    $r = $this->getShortName($parentId, $date, $useHtmlLinkForParent);
                }
                $first = false;
            }

            if ($withHtmlLink && !$showLinkableHierarchy) {
                // add link to the complete name
                return $this->getUrlLinkToViewer($organizationId, $date, $r, false);
            } else {
                return $r;
            }

        } else {
            return $this->getShortName($organizationId, $date, $withHtmlLink);
        }
    }

    protected function signalErrorForOrganizationWithInfiniteLoop($childId)
    {
        $name = $this->getShortName($childId);
        $problemDuplicationKey = "OrganizationUnitInfo - loop in " . $childId;
        $problemDescription = 'The organization/customer/extension `' . $name . '` at id `' . $childId . '` has an infinite loop in his parent hierarchy.';
        $problemEffect = 'CDRs rating is not reliable.';
        $problemProposedSolution = 'Correct the problem in organization/customer specification removing the infinite loop. You can use also the command from the php console "php asterisell.php data delete-organization ' . $childId . '", for deleting completely the info from the database, and replace later with a correct version.';
        ArProblemException::createWithoutGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
    }

    /**
     * A unique identifier referencing this object and his parent chain.
     * Every time a organization has the same parents in a certain point of time,
     * then this identifier is the same value.
     * It can be used for grouping organization according their hierarchy, for example in reports.
     *
     * @param int $organizationId
     * @param int|null $date null for current date
     * @return string|null a unique full identifier composed from the id of all the ar_organization parent, and his id.
     *         null if it does not exists. It has format
     *         "/id1/id2/id3/",
     *         where "id1" is the main root, and "id3" the last child.
     *
     * NOTE: I can not reuse `$this->fromDate`, because parent can have different date of change
     */
    public function getFullIds($organizationId, $date)
    {
        $info = $this->getDataInfo($organizationId, $date);

        if (!$info[self::DATA_STRUCTURE_EXISTS]) {
            return null;
        }

        $r = '';
        $first = true;
        $parentId = $organizationId;
        $avoidLoop = array();

        while (!is_null($info) && $info[self::DATA_STRUCTURE_EXISTS]) {
            $avoidLoop[$parentId] = true;
            $parentName = '/' . $info[self::DATA_AR_ORGANIZATION_UNIT_ID];

            if ($first) {
                $r = $parentName;
                $first = false;
            } else {
                $r = $parentName . $r;
            }

            $childId = $parentId;
            $parentId = $this->getParentId($parentId, $date);
            if (is_null($parentId)) {
                $info = null;
            } else {
                if (array_key_exists($parentId, $avoidLoop)) {
                    $info = null;
                    $parentId = null;
                    $this->signalErrorForOrganizationWithInfiniteLoop($childId);
                }
                $info = $this->getDataInfo($parentId, $date);
            }
        }

        return $r . '/';
    }

    /**
     * @param int $organizationId
     * @return string something like "/10/" where 10 is the organization id
     */
    public static function getSingleIds($organizationId)
    {
        return '/' . $organizationId . '/';
    }

    /**
     * @param string $fullIds
     * @return int 1 for root level (something like '/1/'), 2 for first child (something like '/1/2/') and so on.
     *             0 for bad strings, or the root of all organizations.
     */
    public static function getFullIdsNestingLevel($fullIds)
    {
        $r = substr_count($fullIds, '/');

        if ($r >= 2) {
            $r--;
        } else {
            $r = 0;
        }

        return $r;
    }

    /**
     * @param string $fullIds
     * @return int given something like '/1/2/5/' return 5
     *
     * require: $fullIds has at least an element
     */
    public static function getLastId($fullIds)
    {
        $ids = explode('/', $fullIds);

        $s = count($ids);

        assert($s >= 3);

        return intval($ids[$s - 2]);
    }

    /**
     * @param string $fullIds
     * @param int $position 0 for root level, 1 for first children
     * @return int|null
     */
    public static function getIdInFullIds($fullIds, $position)
    {
        $ids = explode('/', $fullIds);
        if (isset($ids[$position])) {
            return intval($ids[$position]);
        } else {
            return null;
        }
    }


    /**
     * @param int $organizationId
     * @param int|null $date
     * @param string $content
     * @param bool $performHtmlEscape true for executing `htmlentities` on $content
     * @return String
     * @pre $content was submitted to `htmlentities`
     */
    public function getUrlLinkToViewer($organizationId, $date, $content, $performHtmlEscape = true)
    {
        return linkToOrganizationUnitViewer($organizationId, $date, $content, $performHtmlEscape);
    }

    /**
     * @param string $hierarchy a string like "/123/45/456/" identifying the root parent,
     * and other parents in the hierarchy. It is the result of `self::getFullIds()`
     * @return int[] the parent ids, starting from the root parent
     */
    public static function getParentIdsFromCachedParentIdHierarchy($hierarchy)
    {
        $ids = explode('/', $hierarchy);

        $r = array();
        foreach ($ids as $id) {
            if (!isEmptyOrNull($id)) {
                $r[] = intval($id);
            }
        }

        return $r;
    }

    /**
     * @param string $rootIds
     * @param string $childIds
     * @return bool true if $childIds is a proper or improper child of $rootIds
     */
    public static function isChildrenUsingCachedParentIdHierarchy($rootIds, $childIds)
    {
        $rootIdsLen = strlen($rootIds);
        if (strlen($childIds) < $rootIdsLen) {
            return false;
        } else {
            if (strcmp(substr($childIds, 0, $rootIdsLen), $rootIds) == 0) {
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @param bool $withHtmlLink TRUE for inserting a link to the unit viewer,
     *        in this case the name is `applied to htmlentities`
     * @return string a human readable description of the info
     */
    public
    function getShortDescription($organizationId, $date = NULL, $withHtmlLink = FALSE)
    {

        $info = $this->getDataInfo($organizationId, $date);

        $r = $this->getShortName($organizationId, $date) . ' ';

        $dateS = fromUnixTimestampToSymfonyStrTimestamp($info[self::DATA_STRUCTURE_FROM]);

        $showCategory = true;
        if (!$info[self::DATA_STRUCTURE_EXISTS]) {
            $r .= __('was cancelled at date ') . ' ' . $dateS;
            $showCategory = false;
        } else {
            $parentId = $this->getParentId($organizationId, $date);
            if (is_null($parentId)) {
                $r .= __('was created at date ') . ' ' . $dateS;
            } else {
                $r .= __('has parent ') . $this->getShortName($parentId, $date) . ', ' . __('from date ') . ' ' . $dateS;
            }
        }

        if ($showCategory) {
            try {
                $priceCategoryId = $this->getNearestRateCategoryId($organizationId, $date);
                $priceCategory = ArRateCategoryPeer::retrieveByPK($priceCategoryId);
                if (is_null($this->getArRateCategoryId($organizationId, $date))) {
                    $r .= ' ' . __(', with the same price category of his parent') . ' (' . $priceCategory->getName() . ')';
                } else {
                    $r .= ' ' . __(', with price category ') . $priceCategory->getName();
                }
            } catch (ArProblemException $e) {
                $r .= ' ' . __(', with an unspecified rate category');
            }
        }

        if ($withHtmlLink) {
            return $this->getUrlLinkToViewer($organizationId, $date, $r, false);
        } else {
            return $r;
        }
    }

    ///////////////////////////////////
    // Access to Associated Elements //
    ///////////////////////////////////

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int the arRateCategoryId, or the first associated value following the organization hierarchy.
     * @throws ArProblemException
     */
    public function getNearestRateCategoryId($organizationId, $date = null)
    {
        $avoidLoop = array();
        $id = $organizationId;
        while (!is_null($id)) {
            $info = $this->getDataInfo($id, $date);
            $avoidLoop[$id] = true;

            if (is_null($info) || !$info[self::DATA_STRUCTURE_EXISTS]) {
                $id = null;
            } else if (!is_null($info[self::DATA_STRUCTURE_AR_RATE_CATEGORY_ID])) {
                return $info[self::DATA_STRUCTURE_AR_RATE_CATEGORY_ID];
            } else {
                $childId = $id;
                $id = $this->getParentId($id, $date);
                if (array_key_exists($id, $avoidLoop)) {
                    $id = null;
                    $this->signalErrorForOrganizationWithInfiniteLoop($childId);
                }
            }
        }

        // Signal the problem
        $fullName = $this->getFullNameAtDate($organizationId, $date, false, false);

        $problemDuplicationKey = "OrganizationUnitInfo - no rate category for " . $organizationId;
        $problemDescription = 'The organization  `' . $fullName . '` has no rate category.';
        $problemEffect = 'Rates can not be applied to the organization.';
        $problemProposedSolution = 'Specify a rate category for the organization, or one of its parents.';
        throw(ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, CheckOrganizationHierarchy::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution));
    }

    ////////////////////
    // Retrieve Data  //
    ////////////////////

    /**
     * Retrieve a `OrganizationUnitInfo`, and check also for problems.
     *
     * @param int $id ArOrganizationUnit.id
     * @return int|null the most recent date in unix time stamp format,
     */
    public function getFirstDateForOrganizationUnit($id)
    {

        if (array_key_exists($id, $this->idToStructureData)) {
            if (count($this->idToStructureData[$id]) > 0) {
                // element are ordered by date, so the first element is the most recent
                list($info) = $this->idToStructureData[$id];
                return $info[self::DATA_STRUCTURE_FROM];
            }
        }

        return null;
    }

    /**
     * @static
     * @param int $id
     * @return array of (childrenId, fromDate)
     */
    public function getDirectChildrenIds($id)
    {
        if (array_key_exists($id, $this->idToDirectChildrenIds)) {
            return $this->idToDirectChildrenIds[$id];
        } else {
            return array();
        }
    }

    /**
     * @param int $organizationId
     * @return array userId => array of permissionId
     */
    public function getDirectUsersWithPermissions($organizationId)
    {
        if (array_key_exists($organizationId, $this->idToUserPermissions)) {
            return $this->idToUserPermissions[$organizationId];
        } else {
            return array();
        }
    }

    /**
     * @param int $organizationId
     * @return array userId => array of permissionId
     */
    public function getDirectUsersWithAllPermissions($organizationId)
    {


        $users = array();
        if (array_key_exists($organizationId, $this->idToUserPermissions)) {
            foreach ($this->idToUserPermissions[$organizationId] as $userId => $directPermissions) {
                $users[$userId] = true;
            }
        }
        if (array_key_exists($organizationId, $this->idToUserRoles)) {
            foreach ($this->idToUserRoles[$organizationId] as $userId => $directRoles) {
                $users[$userId] = true;
            }
        }

        $r = array();

        foreach ($users as $userId => $ignore) {

            $r[$userId] = array();

            $allPermissions = ArViewAllUserPermissionsPeer::getPermissionsOfUser($userId);
            foreach ($allPermissions as $permission) {
                $r[$userId][] = $permission->getArPermissionId();
            }
        }

        return $r;
    }

    /**
     * @param int $organizationId
     * @return array userId => array of roleId
     */
    public function getDirectUsersWithRoles($organizationId)
    {
        if (array_key_exists($organizationId, $this->idToUserRoles)) {
            return $this->idToUserRoles[$organizationId];
        } else {
            return array();
        }
    }

    /**
     * The users of parent organizations, with permissions.
     *
     * @param int $organizationId
     * @return array parentOrganizationId => array of userId => array of permissionId
     */
    public function getIndirectUsersWithPermissions($organizationId)
    {
        $r = array();
        $avoidLoop = array();
        $parentId = $this->getParentId($organizationId, null);
        while (!is_null($parentId)) {
            $avoidLoop[$parentId] = true;
            $r[$parentId] = $this->getDirectUsersWithPermissions($parentId);

            $childId = $parentId;
            $parentId = $this->getParentId($parentId, null);
            if (array_key_exists($parentId, $avoidLoop)) {
                $this->signalErrorForOrganizationWithInfiniteLoop($childId);
            }
        }

        return $r;
    }

    /**
     * The users of parent organizations, with roles.
     *
     * @param int $organizationId
     * @return array parentOrganizationId => array of userId => array of roleId
     */
    public function getIndirectUsersWithRoles($organizationId)
    {
        $avoidLoop = array();
        $r = array();
        $parentId = $this->getParentId($organizationId, null);
        while (!is_null($parentId)) {
            $avoidLoop[$parentId] = true;

            $r[$parentId] = $this->getDirectUsersWithRoles($parentId);

            $childId = $parentId;
            $parentId = $this->getParentId($parentId, null);
            if (array_key_exists($parentId, $avoidLoop)) {
                $this->signalErrorForOrganizationWithInfiniteLoop($childId);
            }
        }

        return $r;
    }

    /**
     * @param array $info
     * @return bool TRUE if it associated to a party
     */
    public function hasExistentialType($info)
    {
        if (!is_null($info[self::DATA_PARTY_ID])) {
            return TRUE;
        }

        return FALSE;
    }

    ///////////////////////
    // Retrieve Children //
    ///////////////////////

    /**
     * @param int $organizationId
     * @return array with childId => true, for all children in the past, present and future of the organization
     */
    public function getAllChildren($organizationId) {
        $processed = array();
        $stack = array();
        array_push($stack, $organizationId);
        $processed[$organizationId] = true;

        while(count($stack) > 0) {
            $id = array_pop($stack);
            foreach ($this->getAllDirectChildren($id) as $v) {
                list($childId, $fromDate) = $v;
                if (!array_key_exists($childId, $processed)) {
                    // there can be circular links, because an organization can be children of different organizations over time
                    array_push($stack, $childId);
                    $processed[$childId] = true;
                }
            }
        }

        return $processed;
    }

    /**
     * @param int $organizationId
     * @param int|null $fromDate
     * @param int|null $toDate
     * @return array with childId => true, but only for children active in the specified time frame
     */
    public function getAllChildrenAtDate($organizationId, $fromDate, $toDate) {
        $result = $this->getAllChildren($organizationId);

        $childrenToRemove = array();
        foreach($result as $childId => $alwaysTrue) {
            $data = $this->getDataInfo($childId, $fromDate);

            $childDate = $data[self::DATA_STRUCTURE_FROM];
            if (is_null($toDate) || $childDate > $toDate) {
                array_push($childrenToRemove, $childId);
            }
        }

        foreach($childrenToRemove as $childId) {
            unset($result[$childId]);
        }

        return $result;
    }

    /**
     * @param int $organizationId
     * @param int|null $fromDate
     * @param int|null $toDate
     * @param string|null $filterOnInternalNamePrefix insert only extensions with the specified internal_name prefix
     * @param string|null $filterOnNotInternalNamePrefix insert only extensions without the specified internal_name prefix
     * @return array of DIDS as values
     */
    public function getAllDIDSAtDate($organizationId, $fromDate, $toDate, $filterOnInternalNamePrefix = null, $filterOnNotInternalNamePrefix = null) {
        $children = $this->getAllChildrenAtDate($organizationId, $organizationId, $fromDate, $toDate);

        $result = array();
        foreach($children as $childId => $alwaysTrue) {
            $data = $this->getDataInfo($childId, $fromDate);
            if ($data[self::DATA_UNIT_TYPE_IS_LEAF]) {
               $internalName = $data[self::DATA_UNIT_INTERNAL_NAME];
               if (is_null($filterOnInternalNamePrefix)) {
                   $passFilter1 = true;
               } else {
                   $passFilter1 = isPrefixOf($filterOnInternalNamePrefix, $internalName);
               }

               if (is_null($filterOnNotInternalNamePrefix)) {
                  $passFilter2 = true;
               } else {
                   $passFilter2 = ! isPrefixOf($filterOnNotInternalNamePrefix, $internalName);
               }

               if ($passFilter1 && $passFilter2) {
                $v = $data[self::DATA_EXTENSION_NAME];
                if (isEmptyOrNull($v)) {
                   $v = $data[self::DATA_EXTENSION_CODES];
                }
                array_push($result, $v);
               }
            }
        }

        return $result;
    }


    /**
     * @param int $organizationId
     * @return array with pairs (childrenId, fromDate)
     */
    public function getAllDirectChildren($organizationId)
    {
        if (array_key_exists($organizationId, $this->idToDirectChildrenIds)) {
            return $this->idToDirectChildrenIds[$organizationId];
        } else {
            return array();
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @param bool $alsoLeaf TRUE for retrieving also leaf children
     * @return array childId => true for children that exists at the date
     */
    public function getDirectChildrenAtDate($organizationId, $date = NULL, $alsoLeaf = TRUE)
    {
        if (is_null($date)) {
            $date = time();
        }

        if (array_key_exists($organizationId, $this->idToDirectChildrenIds)) {
            $result = array();
            foreach ($this->idToDirectChildrenIds[$organizationId] as $v) {
                list($childId, $fromDate) = $v;

                if (!array_key_exists($childId, $result)) {
                    if ($date >= $fromDate) {
                        $childInfo = $this->getDataInfo($childId, $date);
                        if ($alsoLeaf || (!$childInfo[self::DATA_UNIT_TYPE_IS_LEAF])) {
                            $result[$childId] = true;
                        }
                    }
                }
            }
            return $result;
        } else {
            return array();
        }
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @param bool $alsoLeaf TRUE for retrieving also leaf children
     * @return mixed[] array(array childId => unixtimestamp of children strictly in the future respect $date,
     *                       array childId => unixtimestamp of children strictly in the present respect $date,
     *                       array childId => unixtimestamp of children strictly in the past respect $date).
     */
    public function classifyDirectChildrenAtDate($organizationId, $date, $alsoLeaf = TRUE)
    {
        if (is_null($date)) {
            $date = time();
        }

        // Signal as already processed children in the present
        $presentChildrenSet = $this->getDirectChildrenAtDate($organizationId, $date, $alsoLeaf);
        $beforeChildrenSet = array();
        $afterChildrenSet = array();

        // Classify other children
        if (array_key_exists($organizationId, $this->idToDirectChildrenIds)) {
            foreach ($this->idToDirectChildrenIds[$organizationId] as $v) {
                list($childId, $fromDate) = $v;
                if (!array_key_exists($childId, $beforeChildrenSet) && !array_key_exists($childId, $afterChildrenSet) && !array_key_exists($childId, $presentChildrenSet)) {

                    $childInfo = $this->getDataInfo($childId, $fromDate);

                    if ($alsoLeaf || (!$childInfo[self::DATA_UNIT_TYPE_IS_LEAF])) {
                        if ($fromDate < $date) {
                            $beforeChildrenSet[$childId] = $fromDate;
                        } else if ($fromDate > $date) {
                            $afterChildrenSet[$childId] = $fromDate;
                        }
                    }
                }
            }
        }

        return array($beforeChildrenSet, $presentChildrenSet, $afterChildrenSet);

    }

    /////////////////////////////////////////
    // Retrieve Changes in Classifications //
    /////////////////////////////////////////

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int[] a unixtimestamp for each modification date of the object in the past, in descending order.
     */
    public function getDirectPastModificationDates($organizationId, $date)
    {
        return $this->getDirectPastOrFutureModificationDates($organizationId, $date, true);
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @return int[] a unixtimestamp for each modification date of the object in the future, in descending order.
     */
    public function getDirectFutureModificationDates($organizationId, $date)
    {
        return $this->getDirectPastOrFutureModificationDates($organizationId, $date, false);
    }

    /**
     * @param int $organizationId
     * @param int|null $date
     * @param bool $pastModifications
     * @return int[] a unixtimestamp for each modification date of the object in the past or in the future, in descending order.
     */
    protected function getDirectPastOrFutureModificationDates($organizationId, $date, $pastModifications)
    {
        if (is_null($date)) {
            $date = time();
        }

        $centralInfo = $this->getDataInfo($organizationId, $date);
        $centralDate = $centralInfo[self::DATA_STRUCTURE_FROM];

        $r = array();

        if (array_key_exists($organizationId, $this->idToStructureData)) {
            foreach ($this->idToStructureData[$organizationId] as $info) {
                if ($centralDate != $info[self::DATA_STRUCTURE_FROM] &&
                    ($pastModifications && $info[self::DATA_STRUCTURE_FROM] < $date) || ((!$pastModifications) && $info[self::DATA_STRUCTURE_FROM] > $date)
                ) {
                    $r[] = $info[self::DATA_STRUCTURE_FROM];
                }
            }
        }

        return $r;
    }

    ////////////////////////////////
    // SELECTION OF ORGANIZATIONS //
    ////////////////////////////////

    /**
     * @return array organizationId => true for root organization in the past, present or future.
     */
    public function getMaybeRootOrganizationsIds()
    {
        return $this->maybeRootOrganizations;
    }

    /**
     * @param int $fromDate
     * @param int|null $toDate
     * @return array organizationId => true for root organizations at the current date
     */
    public function getRootOrganizationsIds($fromDate, $toDate)
    {
        if (is_null($toDate)) {
            $toDate = time();
        }

        $r = array();
        foreach ($this->getMaybeRootOrganizationsIds() as $id => $ignore) {
            foreach ($this->idToStructureData[$id] as $rs) {
                if ($rs[self::DATA_STRUCTURE_EXISTS]
                    && is_null($rs[self::DATA_STRUCTURE_AR_PARENT_ORGANIZATION_UNIT_ID])
                    && $rs[self::DATA_STRUCTURE_FROM] < $toDate
                ) {
                    $r[$id] = true;
                    break;
                } else if ($rs[self::DATA_STRUCTURE_FROM] < $fromDate) {
                    break;
                }
            }
        }

        return $r;
    }

    /**
     * @return int|null null if there can be more than one root organization in the system
     */
    public function getUniqueRootOrganizationIdIfExists()
    {
        $vs = array_keys($this->getMaybeRootOrganizationsIds());
        if (count($vs) == 1) {
            list($v) = $vs;
            return $v;
        } else {
            return null;
        }
    }

    /**
     * Prepare an array to use in a select filter, user interface element.
     *
     * @param int|null $rootOrganizationId null for displaying all the root organizations
     * @param bool $includeOrganizations true for showing also proper organizations
     * @param bool $includeExtensions true for showing also extensions
     * @param bool $useExtensionShortCode true for including only the extensions with a defined short code, and use it
     * instead of the extension name
     * @param bool $excludeExtensionsWithoutAName true for excluding extensions without an explicit name.
     * @param bool $forCustomer true for translating
     * @return array
     */
    public function getUIOrganizationSelector($rootOrganizationId, $includeOrganizations = true, $includeExtensions = true, $useExtensionShortCode = false, $excludeExtensionsWithoutAName = false, $forCustomer = false)
    {
        // NOTE: I don't cache the result because there can be changes in the used root organization id

        // TODO: use in future premade caches of recurrent asked values, in order to saving calcs,
        // save the cache result on a file to invalidate when the organization hierarchy change

        $options = array("" => "");

        if ($includeOrganizations || ($includeExtensions && (!is_null($rootOrganizationId)))) {
            // use a top-down approach starting for $rootOrganizationId or from root organizations,
            // for creating a tree like view, or a flat list, but with only the reachable extensions

            $displayedIds = array();
            $toExplore = array();

            $indentChars = '....';

            // Start from root organizations

            $toOrder = array();

            if (is_null($rootOrganizationId)) {
                foreach ($this->getMaybeRootOrganizationsIds() as $childId => $ignore) {
                    $toOrder[$childId] = $this->getShortName($childId, null, false, true, $useExtensionShortCode);
                }
            } else {
                $toOrder[$rootOrganizationId] = $this->getShortName($rootOrganizationId, null, false, true, $useExtensionShortCode);
            }

            arsort($toOrder, SORT_LOCALE_STRING);
            foreach ($toOrder as $childId => $name) {
                array_push($toExplore, $childId);
                array_push($toExplore, $name);
            }

            // Show children for each node

            while (count($toExplore) > 0) {
                $organizationName = array_pop($toExplore);
                $organizationId = array_pop($toExplore);

                if ($this->getExists($organizationId, null)) {
                    $displayedIds[$organizationId] = true;

                    // calculate indent level
                    $indentLevel = '';
                    if ($includeOrganizations) {
                        $avoidLoop = array();
                        $parentId = $this->getParentId($organizationId, null);
                        while (!is_null($parentId)) {
                            $avoidLoop[$parentId] = true;
                            $indentLevel .= $indentChars;
                            $childId = $parentId;
                            $parentId = $this->getParentId($parentId, null);
                            if (array_key_exists($parentId, $avoidLoop)) {
                                $this->signalErrorForOrganizationWithInfiniteLoop($childId);
                                $parentId = null;
                            }
                        }
                    }

                    // the option to show
                    if (!isEmptyOrNull($organizationName)) {

                        $isLeaf = $this->getTypeIsLeaf($organizationId, null);

                        if ($isLeaf) {
                            if ($includeExtensions) {
                                if ($excludeExtensionsWithoutAName) {
                                    $extensionInfo = $this->getDataInfo($organizationId, null);
                                    $includeElement = !$this->isEmptyExtensionName($extensionInfo);
                                } else {
                                    $includeElement = true;
                                }
                            } else {
                                $includeElement = false;
                            }
                        } else {
                            $includeElement = $includeOrganizations;
                        }

                        if ($includeElement) {
                            $options[$organizationId] = $indentLevel . $organizationName;
                        }
                    }

                    // schedule showing of children
                    $toOrder = array();
                    foreach ($this->getDirectChildrenAtDate($organizationId, null, true) as $childId => $ignore) {
                        $toOrder[$childId] = $this->getShortName($childId, null, false, true, $useExtensionShortCode);
                    }

                    arsort($toOrder, SORT_LOCALE_STRING);
                    foreach ($toOrder as $childId => $name) {
                        array_push($toExplore, $childId);
                        array_push($toExplore, $name);
                    }
                }
            }

            if ((!$includeOrganizations) && $includeExtensions) {
                // order extension because they are a flat list, without structure
                asort($options, SORT_LOCALE_STRING);
            }

            //
            // Show not existing elements
            //

            $toOrder = array();
            if (!is_null($rootOrganizationId)) {
                // isolate children of $rootOrganizationId that are not displayed, because defined in the future
                // or in the past.

                $toExplore = array($rootOrganizationId);
                $exploredIds = array();

                while (count($toExplore) > 0) {
                    $idToExplore = array_pop($toExplore);

                    if (!array_key_exists($idToExplore, $exploredIds)) {
                        $exploredIds[$idToExplore] = true;

                        if (!array_key_exists($idToExplore, $displayedIds)) {
                            $displayedIds[$idToExplore] = true;
                            $totOrder[$idToExplore] = $this->getFullNameAtDate($idToExplore, null, false, false);
                        }

                        list($list1, $list2, $list3) = $this->classifyDirectChildrenAtDate($idToExplore, null, true);

                        foreach ($list1 as $id => $ignore) {
                            array_push($toExplore, $id);
                        }

                        foreach ($list2 as $id => $ignore) {
                            array_push($toExplore, $id);
                        }

                        foreach ($list3 as $id => $ignore) {
                            array_push($toExplore, $id);
                        }
                    }
                }

            } else {
                // find elements not displayed
                foreach ($this->idToStructureData as $childId => $ignore) {
                    if (!array_key_exists($childId, $displayedIds)) {
                        $toOrder[$childId] = $this->getFullNameAtDate($childId, null, false, false);
                    }
                }
            }

            asort($toOrder, SORT_LOCALE_STRING);

            foreach ($toOrder as $childId => $name) {
                $displayedIds[$childId] = true;

                if (!isEmptyOrNull($name)) {
                    // the option to show
                    if ($includeOrganizations && $includeExtensions) {
                        $includeElement = true;
                    } else {
                        $isLeaf = $this->getTypeIsLeaf($childId, null);
                        $includeElement = ($includeExtensions && $isLeaf) || ($includeOrganizations && (!$isLeaf));
                    }
                    if ($includeElement) {
                        $options[$childId] = mytr('(not existing anymore)', $forCustomer) . ' ' . $name;
                    }
                }
            }

            return $options;
        } else if ($includeExtensions && is_null($rootOrganizationId)) {
            // include only extensions and not organizations, so use a flat list

            $pastExtensions = array();

            foreach ($this->idToStructureData as $organizationId => $ignore) {
                if ($this->getTypeIsLeaf($organizationId, null)) {
                    $name = $this->getShortName($organizationId, null, false, false, $useExtensionShortCode);
                    if (!isEmptyOrNull($name)) {
                        if ($this->getExists($organizationId, null)) {
                            $options[$organizationId] = $name;
                        } else {
                            $pastExtensions[$organizationId] = mytr('(past extension)', $forCustomer) . ' ' . $name;
                        }
                    }
                }
            }

            asort($options, SORT_LOCALE_STRING);
            asort($pastExtensions, SORT_LOCALE_STRING);

            foreach ($pastExtensions as $id => $value) {
                $options[$id] = $value;
            }

            return $options;

        } else {
            return $options;
        }
    }

    /**
     * Delete a part of organization definition, or the entire organization
     * if this was the last valid definition of the organization.
     * The structure will be physically deleted, so it is a fix/overwrite of the organization history.
     * @param int $structureId the part of structure id to delete
     * @param PDO|null $conn
     * @return bool true if also the main organization was deleted,
     * false if only this organization structure is deleted,
     * @throws Exception signal if there are constraints invalidated, from deleting this organization.
     * The organization is not deleted, and the constraint is raised.
     */
    public function deletePhysicallyUnitStructure($structureId, $conn = null)
    {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $conn->beginTransaction();
        try {

            $structure = ArOrganizationUnitHasStructurePeer::retrieveByPK($structureId, $conn);
            if (is_null($structure)) {
                throw new Exception("Organization Structure with id " . $structureId . " does not exists.");
            }

            $unitId = $structure->getArOrganizationUnitId();

            $howManyDefs = $this->howManyDefinitionsInThePast($unitId);
            if ($howManyDefs < 2) {
                // if there is no history left, then I delete also the organization
                $deleteAlsoOrganization = true;
            } else {
                // otherwise I'm deleting only the piece of structureId
                $deleteAlsoOrganization = false;
            }

            if ($deleteAlsoOrganization) {
                $reason = $this->canOrganizationUnitBeDeleted($unitId, null, $conn);
                if (is_null($reason)) {
                    $this->deletePhysicallyOrganization($unitId, $conn);
                } else {
                  throw new Exception('Can not delete organization with id ' . $unitId . '. Reason: ' . $reason);
                }

            } else {
                $structure->delete($conn);
            }

            self::resetCachedValues();

            $conn->commit();
            return $deleteAlsoOrganization;

        } catch (Exception $e) {
            $conn->rollBack();
            throw($e);
        }
    }

    /**
     * Delete physically an organization, with all its info.
     * @param int $id
     * @param PDO|null $conn
     * @throws Exception
     */
    static public function deletePhysicallyOrganization($id, $conn = null)
    {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $conn->beginTransaction();
        try {
            $query = "DELETE FROM ar_organization_unit_has_structure WHERE ar_organization_unit_id = ?";
            $stm = $conn->prepare($query);
            $stm->execute(array($id));

            $query = "DELETE FROM ar_organization_unit WHERE id = ?";
            $stm = $conn->prepare($query);
            $stm->execute(array($id));

            self::resetCachedValues();

            $conn->commit();
        } catch (Exception $e) {
            $conn->rollBack();

            throw($e);
        }
    }

    /**
     * @param int $unitId
     * @param int|null $fromDate from which date the unit must be (logically) deleted.
     * null for physically deleting the organization.
     *
     * @param PDO|null $conn
     * @return null|string null if the organization can be deleted (logically or phisically)
     * from the system, because there are no any more references to it.
     * The reason because it can not be deleted otherwise.
     *
     * In reality not all references are checked, and also TokuDB engine does not enforce
     * them. So it is far from perfect, but we are in the realm of a user self-sabotating itself,
     * deleting real customers.
     */
    public function canOrganizationUnitBeDeleted($unitId, $fromDate = null, $conn = null)
    {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $isPhysicalDeleted = false;
        if (is_null($fromDate)) {
            $isPhysicalDeleted = true;
            $fromDate = JobProcessor::getGlobalStartingDateForCDRProcessinng();
        }

        // interrupt if there are children

        $children = $this->getDirectChildrenAtDate($unitId, $fromDate, true);
        foreach ($children as $childId => $childExists) {
            if ($childExists) {
                return 'It has children organization or extensions.';
            }
        }

        if ($isPhysicalDeleted) {
            // interrupt if there are users

            if (count($this->getDirectUsersWithAllPermissions($unitId)) > 0) {
                return 'There are web accounts associated to the organization.';
            }
        }

        return null;
    }
}

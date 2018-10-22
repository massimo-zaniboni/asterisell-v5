<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Check for problems in the logica] hierarchy.
 *
 * Some problems that are difficult to check with queries, are returned from the methods of OrganizationUnitInfo.
 */
class CheckOrganizationHierarchy extends FixedJobProcessor
{
    /**
     * This file contains the date of last check of call cost limits.
     */
    const FILE_WITH_LAST_CHECK_DATE = "CheckOrganizationHierarchy";

    public function process()
    {

        $timeFrameInMinutes = sfConfig::get('app_check_cost_limits_after_minutes');
        $checkFile = self::FILE_WITH_LAST_CHECK_DATE;
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {
            ArProblemException::garbageCollect(self::FILE_WITH_LAST_CHECK_DATE, null, null);
            self::checkAllProblems();
        }
    }

    /**
     * Check, and generate problems. They are returned, for showing them in the user interface.
     *
     * Use this method for returning also errors in the user interface.
     *
     * @return ArNewProblem[string] the problems. The string key can be ignored, and it is used only for internal reasons
     */
    public static function checkAllProblems()
    {
        $problems = array();
        $now = time();

        // Check for serious problems first

        if (is_null(ArRateCategoryPeer::retrieveIdByInternalName(ArRateCategory::ID_FOR_NORMAL))) {
            $problemDuplicationKey = "no normal category";
            $problemDescription = 'The default normal price category is missing.';
            $problemEffect = 'New organizations can not be created, because they start with normal price category.';
            $problemProposedSolution = 'None because the price category is added by default after the signal of this problem. Avoid to delete or change the name to the price category in future.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_WARNING, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();

            $r = new ArRateCategory();
            $r->setInternalName(ArRateCategory::ID_FOR_NORMAL);
            $r->save();

            return $problems;
        }

        // Check for hierarchy related problems

        $hierarchy = OrganizationUnitInfo::getInstance();

        $conn = Propel::getConnection();

        // Return problems signaled from hierarchy

        foreach ($hierarchy->getDetectedProblems() as $problem) {
            $problems[$problem->getDuplicationKey()] = $problem;
        }

        // NOTE: check of duplicate extension codes is done during CDR rating.

        // Check for duplicated accountcode
        // nothing to do: there is a constraint on the database schema, about the unique account-code

        // Check if it has more than one existential type

        $query = "SELECT s.ar_organization_unit_id FROM ar_organization_unit_has_structure as s where s.ar_party_id IS NOT NULL AND s.extension_name IS NOT NULL";
        $stm = $conn->prepare($query);
        $stm->execute();
        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
            $unit1 = $hierarchy->getFullNameAtDate($rs[0], $now, false, false);

            $problemDuplicationKey = "OrganizationUnitInfo - Existential type conflicts on " . $rs[0];
            $problemDescription = 'The organization  ' . $unit1 . ' has more than one existential type. It can not be both a Party and an Extension.';
            $problemEffect = 'Reports and other jobs can not work properly.';
            $problemProposedSolution = 'Set the correct existential type for the organization unit.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();
        }
        $stm->closeCursor();

        // Check if it has both a name and an existential type

        $query = "SELECT s.ar_organization_unit_id, s.extension_name
                  FROM ar_organization_unit_has_structure AS s
                  WHERE s.extension_name IS NOT NULL
                  AND (s.ar_party_id IS NOT NULL)";
        $stm = $conn->prepare($query);
        $stm->execute();
        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
            $unit1 = $hierarchy->getFullNameAtDate($rs[0], $now, false, false);

            $problemDuplicationKey = "OrganizationUnitInfo - both name and existential type on " . $rs[0];
            $problemDescription = 'The organization  `' . $unit1 . '` has both a explicit name `' . $rs[1] . '`, and an association to an entity.';
            $problemEffect = 'It is not clear if it is better using the organization name, or the associated entity name.';
            $problemProposedSolution = 'Remove the name associated to the organization unit.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_WARNING, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();
        }
        $stm->closeCursor();

        // Check if a leaf extension has children
        // (note this code is fast because it scan all structures in RAM,
        //  and the check for children existence is usually very fast because it han no associated children).

        foreach ($hierarchy->idToStructureData as $organizationId => $infos) {

            $lastFromDate = null;
            foreach ($infos as $info) {

                if ($info[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF] && $info[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS]) {
                    $currentFromDate = $info[OrganizationUnitInfo::DATA_STRUCTURE_FROM];
                    foreach ($hierarchy->getAllDirectChildren($organizationId) as $childrenAndDate) {
                        list($childrenId, $childrenFromDate) = $childrenAndDate;
                        if ($childrenFromDate >= $currentFromDate && (is_null($lastFromDate) || $childrenFromDate < $lastFromDate)) {
                            $problemDuplicationKey = "OrganizationUnitInfo - leaf has children - " . $info[OrganizationUnitInfo::DATA_STRUCTURE_ID];
                            $problemDescription = 'The organization  `' . $hierarchy->getFullNameAtDate($organizationId, $currentFromDate, false, false) . '` is an extension at date ' . fromUnixTimestampToSymfonyStrTimestamp($currentFromDate) . ', but it has also children organizations or extensions. An extension can not have children. ';
                            $problemEffect = 'Reports and other parts of the code can manage the extension in a not correct way, because it is a bad format.';
                            $problemProposedSolution = 'Fix the error in the organization hierarchy.';
                            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_WARNING, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();
                        }
                    }
                }
                $lastFromDate = $info[OrganizationUnitInfo::DATA_STRUCTURE_FROM];

            }
        }
        $stm->closeCursor();

        // Check if organization has no associated info

        $query = 'SELECT o.id
                  FROM ar_organization_unit AS o
                  LEFT JOIN ar_organization_unit_has_structure AS s
                  ON s.ar_organization_unit_id = o.id
                  WHERE s.id IS NULL';

        $stm = $conn->prepare($query);
        $stm->execute();
        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
            $organization = ArOrganizationUnitPeer::retrieveByPK($rs[0]);

            $problemDuplicationKey = "OrganizationUnitInfo - No structure info on " . $rs[0];
            $problemDescription = 'The organization  unit with id `' . $rs[0] . '` has no associated structure (parent, type, rate category). ';
            $problemEffect = 'The organization can not be used in the system.';
            $problemProposedSolution = 'Add information to the organization.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_WARNING, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();
        }
        $stm->closeCursor();

        // Check if there are root organizations that are not billable

        $query = 'SELECT s.ar_organization_unit_id, s.from
                  FROM ar_organization_unit_has_structure AS s
                  LEFT JOIN ar_party AS p
                  ON s.ar_party_id = p.id
                  WHERE s.ar_parent_organization_unit_id IS NULL
                  AND (p.id IS NULL OR (NOT p.is_billable))';

        $stm = $conn->prepare($query);
        $stm->execute();
        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
            $organizationId = $rs[0];
            $fromDate = fromMySQLTimestampToUnixTimestamp($rs[1]);
            $organization = ArOrganizationUnitPeer::retrieveByPK($organizationId);

            $problemDuplicationKey = "OrganizationUnitInfo - root is not billable - " . $rs[0];
            $problemDescription = 'The root organization  unit with id `' . $rs[0] . '`,and name ' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($organizationId, $fromDate, false, false) . ', at date ' . fromUnixTimestampToSymfonyStrTimestamp($fromDate) . ' is not billable.';
            $problemEffect = 'The calls can be not associeted to a billable organization, and they can be lost, during invoice processing, or other report generations, involving only billable organizations.';
            $problemProposedSolution = 'Make the organization billable, from the specified date.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_WARNING, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();
        }
        $stm->closeCursor();

        // Check if there is an organization with a change on the same date

        $query = 'SELECT s1.id, s2.id, s1.ar_organization_unit_id, s1.from
                  FROM ar_organization_unit_has_structure AS s1,
                  ar_organization_unit_has_structure AS s2
                  WHERE s1.ar_organization_unit_id = s2.ar_organization_unit_id
                  AND s1.id < s2.id
                  AND s1.from = s2.from';

        $stm = $conn->prepare($query);
        $stm->execute();
        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
            $struct1Id = $rs[0];
            $structId2 = $rs[1];
            $organizationId = $rs[2];
            $fromDate = fromMySQLTimestampToUnixTimestamp($rs[3]);

            $problemDuplicationKey = "OrganizationUnitInfo - conflicting from date - " . $organizationId;
            $problemDescription = 'The organization with id `' . $organizationId . '`,and name ' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($organizationId, null, false, false) . ', has two changes in the organization hierarchy at the same date ' . fromUnixTimestampToSymfonyStrTimestamp($fromDate);
            $problemEffect = 'The application can not choose between the two changes in a reliable way, because it is not clear what is the change to select.';
            $problemProposedSolution = 'Decide what is the change to make actual, and delete the other.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_WARNING, ArProblemDomain::VOIP_ACCOUNTS, null, $problemDuplicationKey, self::FILE_WITH_LAST_CHECK_DATE, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
            $problems[$problemDuplicationKey] = ArProblemException::getLastArProblem();
        }
        $stm->closeCursor();


        return $problems;
    }
}

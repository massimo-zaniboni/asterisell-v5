<?php

require 'lib/model/om/BaseArOrganizationUnitHasStructurePeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_organization_unit_has_structure' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArOrganizationUnitHasStructurePeer extends BaseArOrganizationUnitHasStructurePeer {


    /**
     * Delete a part of organization definition, or the entire organization
     * if this was the last valid definition of the organization.
     * @param int $structureId ar_organization_unit_has_structure.id
     * @param PDO|null $conn
     * @return true if also the main organization was deleted,
     * false if only this organization structure is deleted.
     * @throws ArProblemException signal if there are constraints invalidated, from deleting this organization.
     * The organization is not deleted, and the constraint is raised.
     *
     */
    public static function deleteByPK($structureId, $conn = null) {
        $info = OrganizationUnitInfo::getInstance();
        return $info->deleteUnitStructure($structureId, $conn);
    }


} // ArOrganizationUnitHasStructurePeer

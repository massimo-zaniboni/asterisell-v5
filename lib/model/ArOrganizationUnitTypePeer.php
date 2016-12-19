<?php

/**
 * Subclass for performing query and update operations on the 'ar_organization_unit_type' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArOrganizationUnitTypePeer extends BaseArOrganizationUnitTypePeer
{

    /**
     * @static
     * @param string $internalName
     * @return ArOrganizationUnitType|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArOrganizationUnitTypePeer::INTERNAL_NAME, $internalName);

        return ArOrganizationUnitTypePeer::doSelectOne($criteria);
    }
}

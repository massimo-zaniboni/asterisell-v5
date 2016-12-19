<?php

/**
 * Subclass for performing query and update operations on the 'ar_organization_unit' table.
 * @package lib.model
 */ 
class ArOrganizationUnitPeer extends BaseArOrganizationUnitPeer
{

    /**
     * @static
     * @param string $internalName
     * @param PDO|null $conn
     * @return ArOrganizationUnit|null
     */
    public static function retrieveByInternalName($internalName, $conn = null)
    {
        $criteria = new Criteria();
        $criteria->add(ArOrganizationUnitPeer::INTERNAL_NAME, $internalName);

        return ArOrganizationUnitPeer::doSelectOne($criteria, $conn);
    }

    /**
     * @static
     * @param string $internalName
     * @param PDO|null $conn
     * @return ArOrganizationUnit|null
     */
    public static function retrieveByInternalName2($internalName, $conn = null)
    {
        $criteria = new Criteria();
        $criteria->add(ArOrganizationUnitPeer::INTERNAL_NAME2, $internalName);

        return ArOrganizationUnitPeer::doSelectOne($criteria, $conn);
    }

    /**
     * @static
     * @param int $extensionId
     * @param PDO $conn
     * @return ArOrganizationUnit|null
     */
    public static function retrieveByExtensionId($extensionId, $conn = null)
    {
        $criteria = new Criteria();
        $criteria->add(ArOrganizationUnitPeer::AR_EXTENSION_ID, $extensionId);

        return ArOrganizationUnitPeer::doSelectOne($criteria, $conn);
    }

}

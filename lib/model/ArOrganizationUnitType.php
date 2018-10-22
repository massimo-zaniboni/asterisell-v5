<?php

/**
 * Subclass for representing a row from the 'ar_organization_unit_type' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArOrganizationUnitType extends BaseArOrganizationUnitType
{
    /**
     * Used in `internal_name` field.
     * IMPORTANT: in case of changes mantain in synchro with `OrganizationHierarchy`
     * on the Haskell side.
     */
    const ID_FOR_EXTENSION = 'extension';

    /**
     * Used in `internal_name` field.
     */
    const ID_FOR_CUSTOMER = 'customer';

    /**
     * Used in `internal_name` field.
     */
    const ID_FOR_OFFICE = 'office';

    /**
     * Used in `internal_name` field.
     */
    const ID_FOR_EXTERNAL_TEL_NUMBER = 'external';

    /**
     * Used in `internal_name` field.
     */
    const ID_FOR_ROOT = 'root';

    /**
     * Used for `internal_name` field.
     */
    const ID_FOR_GENERIC_ORG = 'org';

    /**
     * Used in `internal_name` field.
     */
    const ID_FOR_SYSTEM = 'system';

    static protected $cachedExtensionId = null;

    /**
     * @static
     * @return int the id of the type associated to the extension type
     */
    public static function getExtensionId() {
      if (is_null(self::$cachedExtensionId)) {
          self::$cachedExtensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(self::ID_FOR_EXTENSION)->getId();
      }
      return self::$cachedExtensionId;
    }

    public function __toString() {
        return $this->getName();
    }
}

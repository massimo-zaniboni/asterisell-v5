<?php

sfLoader::loadHelpers(array('Asterisell'));

/**
 * Subclass for representing a row from the 'ar_organization_unit' table.
 *
 * @package lib.model
 */ 
class ArOrganizationUnit extends BaseArOrganizationUnit
{

    const MANAGED_FROM_USER = 0;
    const MANAGED_FROM_IMPORT_EXTENSIONS = 1;
    const MANAGED_FROM_RATE_CALLS = 2;

    /**
     * @return string
     */
    public function __toString() {
        return $this->getHumanReadableName();
    }

    /**
     * @return string
     */
    public function getHumanReadableName() {
        return OrganizationUnitInfo::getInstance()->getHumanReadableName($this->id, null);
    }
}

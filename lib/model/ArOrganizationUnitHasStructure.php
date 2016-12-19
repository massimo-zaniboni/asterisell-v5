<?php

require 'lib/model/om/BaseArOrganizationUnitHasStructure.php';


/**
 * Skeleton subclass for representing a row from the 'ar_organization_unit_has_structure' table.
 *
 *
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArOrganizationUnitHasStructure extends BaseArOrganizationUnitHasStructure
{

    /**
     * Initializes internal state of ArOrganizationUnitHasStructure object.
     * @see        parent::__construct()
     */
    public function __construct()
    {
        // Make sure that parent constructor is always invoked, since that
        // is where any default values for this object are set.
        parent::__construct();
    }

    public function __toString()
    {
        // XXX to implement
        return "XXX to implement";
    }

    public function save(PropelPDO $conn = null)
    {
        $clearCache = $this->isModified() || $this->isNew();

        if (isEmptyOrNull($this->getExtensionName())) {
            $this->setExtensionName(null);

            if (isEmptyOrNull($this->getExtensionCodes())) {
                $this->setExtensionCodes(null);
            }

            if (isEmptyOrNull($this->getExtensionUserCode())) {
                $this->setExtensionUserCode(null);
            }
         }

        $r = parent::save($conn);

        if ($clearCache) {
            // every time this info change, the cache is not anymore reliable
            OrganizationUnitInfo::resetCachedValues();
        }

        return $r;
    }

} // ArOrganizationUnitHasStructure

<?php

require 'lib/model/om/BaseArUserHasRole.php';


/**
 * Skeleton subclass for representing a row from the 'ar_user_has_role' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArUserHasRole extends BaseArUserHasRole {

    public function save(PropelPDO $conn = null)
    {
        $clearCache = $this->isModified() || $this->isNew();
        $r = parent::save($conn);

        if ($clearCache) {
            // every time this info change, the cache is not anymore reliable
            OrganizationUnitInfo::resetCachedValues();
        }

        return $r;
    }

} // ArUserHasRole

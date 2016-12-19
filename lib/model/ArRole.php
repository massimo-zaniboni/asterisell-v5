<?php

/**
 * Subclass for representing a row from the 'ar_role' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArRole extends BaseArRole
{
    ///////////////////////////////////////////
    // INTERNAL NAMES USED FOR DEFAULT ROLES //
    //////////////////////////////////////////

    // roles are created in
    //
    // > apps/asterisell/lib/jobs/admin/ConfigureDefaultParamsAndSettings.php

    const ADMIN = 'admin';

    const USER = 'user';

    const ACCOUNTANT = 'accountant';

    const NOTIFIED_FOR_CRITICAL_ERRORS = 'notified_for_critical_errors';

    const NOTIFIED_FOR_ERRORS = 'notified_for_errors';

    const NOTIFIED_FOR_WARNINGS = 'notified_for_warnings';

    public function __toString() {
        return $this->getName();
    }

}

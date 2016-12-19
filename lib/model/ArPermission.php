<?php

/**
 *
 * @package lib.model
 */ 
class ArPermission extends BaseArPermission
{
    //////////////////////////
    // Default permissions. //
    //////////////////////////
    //
    // NOTE: they are created inside
    //
    // > `asterisell.php createDefaultParamsAndSettings()` function,
    //
    // and default roles are defined in
    //
    // > apps/asterisell/lib/jobs/admin/ConfigureDefaultParamsAndSettings.php
    //
    // NOTE: id must in inverse order of privilege
    //
    // NOTE: reports can be viewed implicitely, according the type of the report
    //
    // IMPORTANT: do not change these values for systems already in production, because they are used as
    // constant for created IDs. Doing so you will alter the meaning of assigned permissions, for already
    // created users.

    const CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS = 10;

    const CAN_VIEW_SYSTEM_ERRORS = 35;

    const CAN_VIEW_REPORTS = 80;

    const CAN_RECEIVE_EMAILS = 100;

    /**
     * Used mainly during database initialization for loading values.
     *
     * @static
     * @return string[] names associated to types
     */
    public static function getConstNames() {
        return array(
                       self::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS => "can view complete telephone numbers"
                     , self::CAN_VIEW_SYSTEM_ERRORS => "can view system errors"
                     , self::CAN_VIEW_REPORTS => "can view documents"
                     , self::CAN_RECEIVE_EMAILS => "can receive by email new documents"
                        );
    }

    /**
     * Used mainly during database initialization for loading values.
     *
     * @static
     * @return string[] description associated to types
     */
    public static function getConstDescriptions() {
        return array(
          self::CAN_VIEW_COMPLETE_TELEPHONE_NUMBERS => 'Can view the complete external telephone number.'
        , self::CAN_VIEW_SYSTEM_ERRORS => "He is informed about errors of the application, for wich he has proper rights."
        , self::CAN_VIEW_REPORTS => "He can view documents, reports and invoices in confirmed state."
        , self::CAN_RECEIVE_EMAILS => "He is notified by email (or with other notification method), when there are new documents."
        );
    }

    public function __toString() {
        return $this->getName();
    }
}

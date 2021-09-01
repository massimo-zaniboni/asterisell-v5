<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * Call Report related modules are generated at "compile-time", accordig some parameters:
 * - app.yml parameters
 * - customer/admin type of call report to generate
 *
 * This function calcs the fields to show in the call report, and CSV exported files.
 * All the logic about which fields enable or disable, is inside this module.
 * Then other modules show the fields, following the instructions of this module.
 *
 * Every time new fields are added to this function, these files must be updated,
 * in module_templates/call_report_template/ :
 * -  generator.yml
 * - actions.php
 * - _list.php
 * - _list_header.php
 * - exportToCsvSuccess.php
 * - review the generated ar_reports
 *
 * Regarding the generated ar_reports, they should include a description of the fields
 * they contain, and they will be sent only to users with the proper rights.
 *
 * Regarding ar_reports, and call report, there are some app.yml settings influencing
 * the type of shown fields.
 *
 * Regarding call report, it will show only the allowed fields.
 */
class FieldsToShow
{

    const ORGANIZATION_ID = 1;

    const CALL_DIRECTION = 2;

    const EXTERNAL_TELEPHONE_NUMBER = 3;

    const GEOGRAPHIC_LOCATION = 4;

    const OPERATOR_TYPE = 5;

    const CALL_DATE = 6;

    const BILL_SEC = 7;

    const COST = 8;

    const INCOME = 9;

    const EARN = 10;

    const COST_SAVINGS = 11;

    const VENDOR = 12;

    const COMMUNICATION_CHANNEL = 13;

    const COUNT_OF_CALLS = 14;

    const CURRENCY = 15;

    const DEBUG_COST_RATE = 16;

    const DEBUG_INCOME_RATE = 17;

    const DEBUG_BUNDLE_ORGANIZATION_ID = 18;

    const ORGANIZATION_LEVEL = 24;

    const FROM_SOURCE_CDR_ID = 25;
    
    /**
     * @param bool $generateForAdmin true for an administrator, false for the customer otherwise
     * @return array an array with the type of field to show as constant value (not key) of class FieldsToShow .
     *         Note: the order of the fields respect also the expected order on the call report.
     *
     * postcondition: count(result) == number-of-fields-to-show
     */
    static
    public function getFieldsToShowInCallReport($generateForAdmin)
    {

        static $cachedResultForAdmin = null;
        static $cachedResultForCustomer = null;

        if ($generateForAdmin) {
            if (!is_null($cachedResultForAdmin)) {
                return $cachedResultForAdmin;
            }
        } else {
            if (!is_null($cachedResultForCustomer)) {
                return $cachedResultForCustomer;
            }
        }

        $r = array();

        $showIncomeAndEarn = sfConfig::get('app_show_income_and_earn_of_call');
        $onlyOneCall = sfConfig::get('app_cdrs_are_associated_always_to_one_call');
        $showCostSavingsForAdministrator = sfConfig::get('app_show_cost_savings_for_administrator');
        $showCostSavingsForCustomer = sfConfig::get('app_show_cost_savings_for_customer');
        $showCommunicationChannelForAdministrator = sfConfig::get('app_show_communication_channel_for_administrator');
        $showCommunicationChannelForCustomer = sfConfig::get('app_show_communication_channel_for_customer');
        $showVendorForCustomer = sfConfig::get('app_show_vendor_for_customer');

        $r[] = FieldsToShow::ORGANIZATION_ID;

        if (!$onlyOneCall) {
            $r[] = FieldsToShow::COUNT_OF_CALLS;
        }

        if ($generateForAdmin || sfConfig::get('app_show_call_direction')) {
            $r[] = FieldsToShow::CALL_DIRECTION;
        }

        $r[] = FieldsToShow::EXTERNAL_TELEPHONE_NUMBER;

        $r[] = FieldsToShow::GEOGRAPHIC_LOCATION;

        $r[] = FieldsToShow::OPERATOR_TYPE;

        $r[] = FieldsToShow::CALL_DATE;

        $r[] = FieldsToShow::BILL_SEC;

        $r[] = FieldsToShow::CURRENCY;

        if ($generateForAdmin) {
            if ($showIncomeAndEarn) {
                $r[] = FieldsToShow::INCOME;
                $r[] = FieldsToShow::COST;
                $r[] = FieldsToShow::EARN;
            } else {
                $r[] = FieldsToShow::COST;
            }
        } else {
            if ($showIncomeAndEarn) {
                $r[] = FieldsToShow::INCOME;
            } else {
                $r[] = FieldsToShow::COST;
            }
        }

        if (($generateForAdmin && $showCostSavingsForAdministrator) ||
                (!$generateForAdmin && $showCostSavingsForCustomer)
        ) {
            $r[] = FieldsToShow::COST_SAVINGS;
        }

        if ($generateForAdmin || $showVendorForCustomer) {
            $r[] = FieldsToShow::VENDOR;
        }

        if (($generateForAdmin && $showCommunicationChannelForAdministrator) ||
                (!$generateForAdmin && $showCommunicationChannelForCustomer)
        ) {
            $r[] = FieldsToShow::COMMUNICATION_CHANNEL;
        }

        if ($generateForAdmin) {
            $cachedResultForAdmin = $r;
        } else {
            $cachedResultForCustomer = $r;
        }

        return $r;
    }

    /**
     * @param bool $generateForAdmin true for an administrator, false for the customer otherwise
     * @return array an array with the type of field to show as constant value (not key) of class FieldsToShow .
     *         Note: the order of the fields respect also the expected order on the call report.
     *
     * postcondition: count(result) == number-of-fields-to-show
     */
    static
    public function getFieldsToShowInCSVReport($generateForAdmin)
    {
        $showIncomeAndEarn = sfConfig::get('app_show_income_and_earn_of_call');

        $r = self::getFieldsToShowInCallReport($generateForAdmin);

        if ($generateForAdmin) {
            $r[] = self::FROM_SOURCE_CDR_ID;

            if ($showIncomeAndEarn) {
                $r[] = self::DEBUG_COST_RATE;
                $r[] = self::DEBUG_INCOME_RATE;
                $r[] = self::DEBUG_BUNDLE_ORGANIZATION_ID ;
            } else {
                // TODO add more fields in case of switch/refactoring to field INCOME instead of COST
                $r[] = self::DEBUG_COST_RATE;
            }
        }

        $r[] = FieldsToShow::ORGANIZATION_LEVEL;

        return $r;
    }

        /**
     * @param bool $generateForAdmin
     * @param int $fieldId
     * @return bool
     */
    static
    public function isFieldToShow($generateForAdmin, $fieldId)
    {
        return in_array($fieldId, self::getFieldsToShowInCallReport($generateForAdmin));
    }

    /**
     * @param bool $generateForAdmin
     * @return int
     */
    static
    public function getCountFieldsToShow($generateForAdmin)
    {
        return count(self::getFieldsToShowInCallReport($generateForAdmin));
    }

    static
    protected function getModuleParams($generateForAdmin)
    {

        $moduleSuffix = "call_report";
        $suffixClassName = "call_reportActions";
        if ($generateForAdmin) {
            $prefixClassName = "admin";
            $prefixParentClassName = "autoAdmin";
            $modulePrefix = "admin";
        } else {
            $prefixClassName = "customer";
            $prefixParentClassName = "autoCustomer";
            $modulePrefix = "customer";
        }

        $className = $prefixClassName . "_" . $suffixClassName;
        $parentClassName = $prefixParentClassName . "_" . $suffixClassName;

        $moduleName = $modulePrefix . "_" . $moduleSuffix;

        return array($className, $parentClassName, $moduleName);
    }

    /**
     * @param bool $generateForAdmin
     * @return string
     */
    static
    public function getModuleName($generateForAdmin)
    {

        list($className, $parentClassName, $moduleName) = self::getModuleParams($generateForAdmin);
        return $moduleName;
    }

    /**
     * @param bool $generateForAdmin
     * @return string
     */
    static
    public function getClassName($generateForAdmin)
    {

        list($className, $parentClassName, $moduleName) = self::getModuleParams($generateForAdmin);
        return $className;
    }

    /**
     * @param bool $generateForAdmin
     * @return string
     */
    static
    public function getParentClassName($generateForAdmin)
    {

        list($className, $parentClassName, $moduleName) = self::getModuleParams($generateForAdmin);
        return $parentClassName;
    }

    /**
     * @return Void
     */
    static
    public function exportToGlobalSettingsTable()
    {
        $conn = Propel::getConnection();
        $conn->exec("DELETE FROM ar_global_permissions WHERE true");

        $s = new ArGlobalPermissions();
        $s->setShowCallCost(false);
        $s->setShowCallIncome(false);
        $s->setShowOutgoingCalls(sfConfig::get('app_show_outgoing_calls'));
        $s->setShowIncomingCalls(sfConfig::get('app_show_incoming_calls'));
        $s->setShowInternalCalls(sfConfig::get('app_show_internal_calls'));
        $s->setShowVoipProvider(false);
        $s->setShowCommunicationChannel(false);
        $s->setShowCostSaving(false);

        $fieldsToShow = FieldsToShow::getFieldsToShowInCallReport(false);

        foreach ($fieldsToShow as $fieldToShow) {
            switch ($fieldToShow) {
                case FieldsToShow::ORGANIZATION_ID:
                    break;

                case FieldsToShow::CALL_DIRECTION:
                    break;

                case FieldsToShow::EXTERNAL_TELEPHONE_NUMBER:
                    break;

                case FieldsToShow::GEOGRAPHIC_LOCATION:
                    break;

                case FieldsToShow::OPERATOR_TYPE:
                    break;

                case FieldsToShow::CALL_DATE:
                    break;

                case FieldsToShow::BILL_SEC:
                    break;

                case FieldsToShow::COST:
                    $s->setShowCallCost(true);
                    break;

                case FieldsToShow::INCOME:
                    $s->setShowCallIncome(true);
                    break;

                case FieldsToShow::EARN:

                    break;

                case FieldsToShow::COST_SAVINGS:
                    $s->setShowCostSaving(true);
                    break;

                case FieldsToShow::VENDOR:
                    $s->setShowVoipProvider(true);
                    break;

                case FieldsToShow::COMMUNICATION_CHANNEL:
                    $s->setShowCommunicationChannel(true);
                    break;

                case FieldsToShow::COUNT_OF_CALLS:
                    break;
                
                case FieldsToShow::FROM_SOURCE_CDR_ID:
                    break;

                case FieldsToShow::ORGANIZATION_LEVEL:
                    break;

                case FieldsToShow::DEBUG_COST_RATE:
                    break;

                case FieldsToShow::DEBUG_INCOME_RATE:
                    break;

                case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
                    break;

                case FieldsToShow::CURRENCY:
                    break;

                default:
                    die('Not supported FieldShow case in class ' . FieldsToShow);
                    break;
            }
        }

        $s->save();

    }

}

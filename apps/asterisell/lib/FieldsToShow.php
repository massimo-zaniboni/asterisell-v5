<?php

/* $LICENSE 2013:
 *
 * Copyright (C) 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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

    const DEBUG_RESIDUAL_INCOME_RATE = 18;

    const DEBUG_RESIDUAL_CALL_DURATION = 19;

    const DEBUG_BUNDLE_ORGANIZATION_ID = 20;

    const DEBUG_BUNDLE_LEFT_CALLS = 21;

    const DEBUG_BUNDLE_LEFT_DURATION = 22;

    const DEBUG_BUNDLE_LEFT_COST = 23;

    const ORGANIZATION_LEVEL = 24;

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
            if ($showIncomeAndEarn) {
                $r[] = self::DEBUG_COST_RATE;
                $r[] = self::DEBUG_INCOME_RATE;
                $r[] = self::DEBUG_RESIDUAL_CALL_DURATION;
                $r[] = self::DEBUG_BUNDLE_ORGANIZATION_ID ;
                $r[] = self::DEBUG_BUNDLE_LEFT_CALLS;
                $r[] = self::DEBUG_BUNDLE_LEFT_DURATION;
                $r[] = self::DEBUG_BUNDLE_LEFT_COST;
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

                case FieldsToShow::ORGANIZATION_LEVEL:
                    break;

                case FieldsToShow::DEBUG_COST_RATE:
                    break;

                case FieldsToShow::DEBUG_INCOME_RATE:
                    break;

                case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
                    break;

                case FieldsToShow::DEBUG_RESIDUAL_CALL_DURATION:
                    break;

                case FieldsToShow::DEBUG_BUNDLE_ORGANIZATION_ID:
                    break;

                case FieldsToShow::DEBUG_BUNDLE_LEFT_CALLS:
                    break;

                case FieldsToShow::DEBUG_BUNDLE_LEFT_DURATION:
                    break;

                case FieldsToShow::DEBUG_BUNDLE_LEFT_COST:
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

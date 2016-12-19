<?php

/* $LICENSE 2009, 2010:
 *
 * Copyright (C) 2009, 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

sfLoader::loadHelpers(array('I18N'));

/**
 * Describe different types associated to the Call Detail Records.
 */
abstract class DestinationType
{

    // IMPORTANT: if you change the value of these constant you must update also Haskell Engine Rating code,
    // and "scripts/call_flow_merge_tools/CProceduresGenerator.php" file.

    const incoming = 1;

    const outgoing = 2;

    const internal = 3;

    const ignored = 4;

    const error = 5;

    const system = 6;

    /**
     * Used during rating-processing for signaling a known (already signaled)
     * problem. It will never be stored inside the CDR in the database,
     * it is only a comunication value.
     */
    const known_error = 5;

    static public $names = array(
        1 => "incoming",
        2 => "outgoing",
        3 => "internal",
        4 => "ignored",
        5 => "error",
        6 => "system"
    );

    static public $namesWithRedirect = array(
        1 => "redirected to incoming",
        2 => "redirected to outgoing",
        3 => "redirected to internal",
        4 => "redirected to ignored",
        5 => "redirected to error",
        6 => "redirected to system"
    );

    /**
     * HTML symbols for each type of calls.
     * They are displayed mainly in CALL REPORT.
     */
    static public $symbols = array(
        1 => "&larr;",
        2 => "&rarr;",
        3 => "&harr;",
        4 => "ignored",
        5 => "error",
        6 => "system"
    );

    /**
     * HTML symbols for each type of calls.
     * They are displayed mainly in CALL REPORT.
     */
    static public $symbolsWithRedirect = array(
        1 => "&hellip;&larr;",
        2 => "&hellip;&rarr;",
        3 => "&hellip;&harr;",
        4 => "redirected to ignored",
        5 => "redirected to error",
        6 => "redirected to system"
    );


    /**
     * @param int $typeValue
     * @param bool $isRedirect
     * @return string the user readable name of the type.
     */
    static public function getName($typeValue, $isRedirect = false)
    {
        if (is_null($typeValue)) {
            return '<null>';
        } else {
            if ($isRedirect) {
                return mytr(self::$namesWithRedirect[$typeValue]);
            } else {
              return mytr(self::$names[$typeValue]);
            }
        }
    }

    /**
     * @static
     * @param int $typeValue
     * @param bool $isRedirect
     * @return string
     */
    static public function getSymbol($typeValue, $isRedirect = false)
    {
        if ($isRedirect) {
            return self::$symbolsWithRedirect[$typeValue];
        } else {
            return self::$symbols[$typeValue];
        }
    }

    /**
     * @static
     * @param int $typeValue
     * @param bool $throwError true for signaling problems wit an exception
     * @return string
     * @throws ArProblemException
     */
    static public function getUntraslatedName($typeValue, $throwError = true)
    {
        if (array_key_exists($typeValue, DestinationType::$names)) {
            return DestinationType::$names[$typeValue];
        } else {
            if ($throwError) {
            $e = new Exception();

            $problemDuplicationKey = "error in code - DestinationType";

            $problemDescription = "The index \"$typeValue\" does not exists in DestinationType. " . $e->getTraceAsString();
            $problemEffect = "The current job is not executed.";
            $problemProposedSolution = "Contact the assistance, because this is a bug in the code.";
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
           } else {
             return "unknown value";
           }
        }
    }

    /**
     * Add to the condition the implicit filters on destination type
     * according "show_incoming/outgoing/internal_calls" settings.
     *
     * @param Criteria $c
     */
    static public function addCustomerFiltersAccordingConfiguration(Criteria $c)
    {
        $allowedTypes = array();

        if (sfConfig::get('app_show_incoming_calls')) {
            array_push($allowedTypes, DestinationType::incoming);
        }

        if (sfConfig::get('app_show_outgoing_calls')) {
            array_push($allowedTypes, DestinationType::outgoing);
        }

        if (sfConfig::get('app_show_internal_calls')) {
            array_push($allowedTypes, DestinationType::internal);
        }

        // NOTE: "system" type is never allowed for customers.

        $c2 = NULL;
        foreach ($allowedTypes as $dt) {
            /**
             * @var Criteria $c2
             */
            if (is_null($c2)) {
                $c2 = $c->getNewCriterion(ArCdrPeer::DESTINATION_TYPE, $dt, Criteria::EQUAL);
            } else {
                $c2->addOr($c->getNewCriterion(ArCdrPeer::DESTINATION_TYPE, $dt, Criteria::EQUAL));
            }
        }

        if (!is_null($c2)) {
            $c->add($c2);
        }
    }

    /**
     * Add to the condition the implicit filters on destination type
     * according the needs of the admin.
     *
     * @param Criteria $c
     */
    static public function addAdminFiltersAccordingConfiguration(Criteria $c)
    {
        $allowedTypes = array();

        array_push($allowedTypes, DestinationType::incoming);
        array_push($allowedTypes, DestinationType::outgoing);
        array_push($allowedTypes, DestinationType::internal);
        array_push($allowedTypes, DestinationType::system);

        $c2 = NULL;
        foreach ($allowedTypes as $dt) {
            /**
             * @var Criteria $c2
             */

            if (is_null($c2)) {
                $c2 = $c->getNewCriterion(ArCdrPeer::DESTINATION_TYPE, $dt, Criteria::EQUAL);
            } else {
                $c2->addOr($c->getNewCriterion(ArCdrPeer::DESTINATION_TYPE, $dt, Criteria::EQUAL));
            }
        }

        if (!is_null($c2)) {
            $c->add($c2);
        }
    }

    //
    // Info About Vendor Cost
    //

    const VENDOR_COST_DIFFERENT_FROM_EXPECTED = 'vendor_cost_unexpected';
    const VENDOR_COST_UNFILTERED = 'vendor_cost_unfiltered';
}

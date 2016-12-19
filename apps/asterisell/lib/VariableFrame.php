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

/**
 * Contains static variables references.
 *
 * I am using this class to transfer variables
 * because report/actions.class.php does not support
 * the traditional way to pass variable from action to view,
 * or I haven't found a suitable method for doing this...
 */
class VariableFrame
{

    const CREATE_NEW_PARTY_ID = '-- Create New --';
    const USE_ORGANIZATION_PARTY_ID = '-- Use Associated Organization Info --';

    // Call report related
    //
    public static $showChannelUsage;

    /**
     * @var Criteria
     */
    public static $filterCondition;

    /**
     * @var Criteria
     */
    public static $filterConditionWithOrder;

    public static $startFilterDate;
    public static $endFilterDate;
    public static $defaultTimeFrameValue;
    public static $countOfRecords;
    public static $totSeconds;
    public static $totIncomes;
    public static $totCosts;
    public static $totSavingCosts;
    public static $totEarn;
    public static $filterOnShow;
    public static $geographicLocationsInTimeRange;
    public static $startOrganizationId;
    public static $showMaskedTelephoneNumbers;

    /**
     * @var string
     */
    public static $filterDescription;

    /**
     * @var ArNumberPortabilityCache|null
     */
  protected static $numberPortabilityCache = null;

    /**
     * @var PhpTelephonePrefixesCache|null
     */
  protected static $telephonePrefixCache= null;

    /**
     * @static
     * @return PhpTelephonePrefixesCache
     */
    static public function getTelephonePrefixCache() {
      if (is_null(VariableFrame::$telephonePrefixCache)) {
        VariableFrame::$telephonePrefixCache = new PhpTelephonePrefixesCache();
      }

      return VariableFrame::$telephonePrefixCache;
    }

    static public function resetTelephonePrefixCache() {
        VariableFrame::$telephonePrefixCache = null;
    }

    /**
     * @static
     * @param int $id
     * @return string
     */
    static public function getCommunicationChannelName($id) {
        static $cache = null;

        if (is_null($cache)) {
            // they are few codes, so I can retrieve and store them using only one query

            $cache = array();
            $conn = Propel::getConnection();
            $stm = $conn->prepare("SELECT id, name FROM ar_communication_channel_type");
            $stm->execute();
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
                $cache[$rs[0]] = $rs[1];
            }
            $stm->closeCursor();
        }

        return $cache[$id];
    }
}
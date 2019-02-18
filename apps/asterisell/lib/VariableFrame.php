<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

    // --------------------------
    // Info passed to call-report

    /**
     * @var int
     */
    public static $fromDate;

    /**
     * @var int|null
     */
    public static $toDate;

    /**
     * @var int
     */
    public static $countOfRecords;

    /**
     * @var bool
     */
    public static $srcCanBeGrouped;

    /**
     * @var bool
     */
    public static $isWholeDay;

   /**
     * @var int
     */
    public static $groupOn;

    /**
     * @var bool true if there is a filter on some field involving organizations,
     * false if all organizations are considered, and so cached_parent_id_hierarchy = ''
     * and billable_ar_organization_unit_id = 0 can be used in ar_cached_grouped_cdr
     */
    public static $filterOnOrganization;

    /**
     * @var bool true for showing fields like call direction, geographic location, and so on.
     */
    public static $showCallDetails;

   /**
     * @var array the SQL query with the filter conditions for showing all the CDRS.
     * Every element of the array is an AND condition to add to the query.
     * It is the WHERE part, without the SELECT and FROM part.
     * The GROUP part is implicit by the used $listViewName.
     */
    public static $listCondition;

   /**
     * @var array the SQL query with the filter conditions for showing the sum/header of the CDRS.
     * Every element of the array is an AND condition to add to the query.
     * It is the WHERE part, without the SELECT and FROM part.$
     * The GROUP part is implicit by the used $listViewName.
     */
    public static $listHeaderCondition;

    /**
     * @var array the params of $listCondition
     */
    public static $listHeaderParams;

    /**
     * @var string
     */
    public static $listFrom;

    /**
     * @var array
     */
    public static $listGroupBy;

    /**
     * @var array SQL sort conditions
     */
    public static $listSort;

    /**
     * @var array the params of $listCondition
     */
    public static $listParams;

    /**
     * @var array
     */
    public static $listSelect;

    /**
     * @var array
     */
    public static $exportToCSVSelect;

    /**
     * @var int
     */
    public static $startOrganizationId;

    /**
     * @var bool
     */
    public static $showMaskedTelephoneNumbers;

    /**
     * @var bool
     */
    public static $showCommunicationChannel;

    /**
     * @var bool false if only outgoing calls are showed
     */
    public static $showMoreDirections;

    /**
     * @var array
     */
    public static $headerTable;

    /**
     * @var array
     */
    public static $headerTotals;

    /**
     * @var array
     */
    public static $headerColNames;

    /**
     * @var array for each col contains 0 for a simple number, 1 for monetary value, 2 for hours/minutes
    */
    public static $headerColFormat;

   /**
    * @var array values in order of appareance
    */
    public static $headerColOrder;

    /**
     * @var array values in order of appareance
     */
    public static $headerRowOrder;

   /**
    * @var array
    */
   public static $headerRowNames;

   /**
     * @var array
     */
   public static $filterOnOperatorType;

    /**
     * @var array
     */
    public static $filterOnCommunicationChannel;

    /**
     * @var array
     */
    public static $filterOnGeographicLocation;

    /**
     * @var array
     */
    public static $filterOnVendor;

    /**
     * @var string
     */
    public static $filterDescription;




}


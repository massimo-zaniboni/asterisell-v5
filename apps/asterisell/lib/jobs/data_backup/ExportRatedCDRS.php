<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2020 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Save on file the rated CDRS for reconciliation porpouses.
 */
class ExportRatedCDRS extends FixedJobProcessor {

    /**
     * @var int|null
     */
    public $fromTime = null;

    public function process() {
        if (is_null($this->fromTime)) {
            $this->fromTime = $this->getOfficialCallDate();
        }

        FixedJobProcessor::getFetchConnection()->beginTransaction();

        $query = '
        SELECT
            cdr.id
          , cdr.calldate
          , cdr.is_service_cdr
          , cdr.to_calldate
          , cdr.count_of_calls
          , cdr.destination_type
          , cdr.is_redirect
          , cdr.duration
          , cdr.billsec
          , cdr.ar_organization_unit_id
          , cdr.cached_parent_id_hierarchy
          , cdr.billable_ar_organization_unit_id
          , u.internal_name AS u_internal_name
          , cdr.bundle_ar_organization_unit_id
          , cdr.income
          , cdr.cost_saving
          , cdr.ar_vendor_id
          , cdr.ar_communication_channel_type_id
          , cdr.cost
          , cdr.expected_cost
          , cdr.ar_telephone_prefix_id
          , tp.geographic_location AS tp_geographic_location
          , tp.operator_type AS tp_operator_type
          , cdr.cached_external_telephone_number
          , cdr.external_telephone_number_with_applied_portability
          , cdr.cached_masked_external_telephone_number
          , cdr.error_destination_type
          , cdr.ar_problem_duplication_key
          , cdr.debug_cost_rate
          , cdr.debug_income_rate
        FROM ar_cdr AS cdr 
        LEFT JOIN ar_organization_unit AS u 
        ON cdr.billable_ar_organization_unit_id = u.id
        LEFT JOIN ar_telephone_prefix AS tp
        ON cdr.ar_telephone_prefix_id = tp.id
        WHERE cdr.calldate >= ?
        AND cdr.destination_type <> ?
        AND cdr.destination_type <> ?
        ORDER BY cdr.calldate
        ';

        $stm = FixedJobProcessor::prepareFetchStmt($query);
        $isOk = $stm->execute(array(
            fromUnixTimestampToMySQLTimestamp($this->fromTime),
            DestinationType::ignored,
            DestinationType::error
        ));

        if ($isOk === FALSE) {
            throw $this->createError(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    'error in query ' . $query,
                    "Error in query \"" . $query . "\"",
                    "CDRs will be not exported",
                    "This is an error in the code. Contact the assistance."
            );
        }

        // Write header
        echo csv_field('cdr_id', true);
        echo csv_field('calldate', false);
        echo csv_field('billed_customer_crm', false);
        echo csv_field('billed_customer_internal_name', false);
        echo csv_field('internal_ids', false);
        echo csv_field('customer', false);
        echo csv_field('destination_type', false);
        echo csv_field('is_service_cdr', false);
        echo csv_field('cached_masked_external_telephone_number', false);
        echo csv_field('external_telephone_number_with_applied_portability', false);
        echo csv_field('geographic_location', false);
        echo csv_field('operator_type', false);
        echo csv_field('billsec', false);
        echo csv_field('cost', false);
        echo csv_field('income', false);
        echo csv_field('earn', false);
        echo csv_field('cost_saving', false);
        echo csv_field('vendor_name', false);
        echo csv_field('communication_channel', false);
        echo csv_field('count_of_calls', false);
        echo csv_field('debug_cost_rate');
        echo csv_field('debug_income_rate');
        echo csv_field('bundle_customer');

        while ($r = $stm->fetch(PDO::FETCH_ASSOC)) {
            echo "\n";
            $cdrDate = fromMySQLTimestampToUnixTimestamp($r['calldate']);
            $unitId = $r['ar_organization_unit_id'];
            $billableUnitId = $r['billable_ar_organization_unit_id'];
            $ids = $r['cached_parent_id_hierarchy'];
            $crm = OrganizationUnitInfo::getInstance()->getPartyCRM($billableUnitId, $cdrDate);
            if (is_null($crm)) {
                $crm = '';
            }

            echo csv_field($r['id'], true);
            echo csv_field($r['calldate'], false);
            echo csv_field($crm, false);
            echo csv_field($r['u_internal_name'], false);
            echo csv_field($ids, false);
            echo csv_field(OrganizationUnitInfo::getInstance()->getFullNameAtDate($unitId, $cdrDate, false, false, null, false), false);
            echo csv_field(DestinationType::getUntraslatedName($r['destination_type'], $r['is_redirect']), false);
            echo csv_field($r['is_service_cdr'], false);
            echo csv_field($r['cached_masked_external_telephone_number'], false);
            echo csv_field($r['external_telephone_number_with_applied_portability'], false);
            echo csv_field($r['tp_geographic_location'], false);
            echo csv_field($r['tp_operator_type'], false);
            echo csv_field($r['billsec'], false);
            echo csv_numeric_field(from_db_decimal_to_php_decimal($r['cost']), false);
            echo csv_numeric_field(from_db_decimal_to_php_decimal($r['income']), false);
            $earn = $r['income'] - $r['cost'];
            echo csv_numeric_field(from_db_decimal_to_php_decimal($earn), false);
            echo csv_numeric_field(from_db_decimal_to_php_decimal($r['cost_saving']), false);
            $vendor = ArVendorPeer::retrieveByPK($r['ar_vendor_id']);
            if (is_null($vendor)) {
                $vendorName = '';
            } else {
                $vendorName = $vendor->getName();
            }
            echo csv_field($vendorName, false);
            echo csv_field(ArCommunicationChannelTypePeer::getInternalName($r['ar_communication_channel_type_id']), false);
            echo csv_field($r['count_of_calls'], false);
            echo csv_field($r['debug_cost_rate']);
            echo csv_field($r['debug_income_rate']);

            $id = $r['bundle_ar_organization_unit_id'];
            if (is_null($id)) {
                $n = '';
            } else {
                $n = OrganizationUnitInfo::getInstance()->getFullNameAtDate($id, $cdrDate, false, false, null, false);
            }
            echo csv_field($n, false);
        }

        $stm->closeCursor();

        $this->commitTransactionOrSignalProblem(FixedJobProcessor::getFetchConnection());

        return '';
    }

}

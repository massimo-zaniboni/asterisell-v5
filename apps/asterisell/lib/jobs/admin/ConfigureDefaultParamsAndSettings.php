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

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


class ConfigureDefaultParamsAndSettings extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        self::createDefaultParamsAndSettings();
        $job = CustomCDRServices::getInstance()->getRateFormatsJob();
        $job->process();

        return '';
    }

    /**
     * Create default params and settings that must be used from the majority of Asterisell applications.
     *
     * @return int default params id
     */
    public static function createDefaultParamsAndSettings()
    {

        $paramsId = self::getOrCreateDefaultParams();

        self::configureLegalDateGenerationMethod();

        ////////////////////////////////
        // Create default error types //
        ////////////////////////////////

        $ps = ArProblemType::getTypeNames();
        foreach ($ps as $id => $name) {
            $p = new ArProblemType();
            $p->setId($id);
            $p->setName($name);
            $p->save();
        }

        $ps = ArProblemDomain::getTypeNames();
        foreach ($ps as $id => $name) {
            $p = new ArProblemDomain();
            $p->setId($id);
            $p->setName($name);
            $p->save();
        }

        $ps = ArProblemResponsible::getTypeNames();
        foreach ($ps as $id => $name) {
            $p = new ArProblemResponsible();
            $p->setId($id);
            $p->setName($name);
            $p->save();
        }

        //////////////////////////////////////
        // Create default call destinations //
        //////////////////////////////////////

        foreach (DestinationType::$names as $id => $name) {
            $p = new ArDestinationType();
            $p->setInternalId($id);
            $p->setName($name);
            $p->save();
        }

        ////////////////////////////////
        // Create default permissions //
        ////////////////////////////////

        $ps = ArPermission::getConstNames();
        $ds = ArPermission::getConstDescriptions();
        foreach ($ps as $id => $name) {
            $description = '';
            if (array_key_exists($id, $ds)) {
                $description = $ds[$id];
            }

            $p = new ArPermission();
            $p->setId($id);
            $p->setName($name);
            $p->setDescription($description);
            $p->setPower($id);
            $p->save();
        }

        //////////////////////////
        // Create default roles //
        //////////////////////////

        $p = new ArRole();
        $p->setName('admin');
        $p->setDescription('');
        $p->setPower(10);
        $p->setInternalName(ArRole::ADMIN);
        $p->save();
        $roleId = $p->getId();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_REPORTS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_RECEIVE_EMAILS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_SYSTEM_ERRORS);
        $p->setArRoleId($roleId);
        $p->save();

        // new role

        $p = new ArRole();
        $p->setName('user');
        $p->setDescription('A normal user of the system, who can view documents and calls.');
        $p->setPower(25);
        $p->setInternalName(ArRole::USER);
        $p->save();
        $roleId = $p->getId();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_REPORTS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_RECEIVE_EMAILS);
        $p->setArRoleId($roleId);
        $p->save();

        // new role

        $p = new ArRole();
        $p->setName('accountant');
        $p->setDescription('Usually he can receive all the invoices. So Invoices must be generated, using the accountant tag/role.');
        $p->setPower(30);
        $p->setInternalName(ArRole::ACCOUNTANT);
        $p->save();
        $roleId = $p->getId();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_RECEIVE_EMAILS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_REPORTS);
        $p->setArRoleId($roleId);
        $p->save();

        // note: the accountant role is used as a tag/flag for sending all the invoices
        // to accountants.

        // new role

        $p = new ArRole();
        $p->setName('Notified for critical errors');
        $p->setDescription('Receive reports about critical errors of the system.');
        $p->setPower(30);
        $p->setInternalName(ArRole::NOTIFIED_FOR_CRITICAL_ERRORS);
        $p->save();
        $roleId = $p->getId();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_RECEIVE_EMAILS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_REPORTS);
        $p->setArRoleId($roleId);
        $p->save();

        // new role

        $p = new ArRole();
        $p->setName('Notified for errors');
        $p->setDescription('Receive reports about important but not critical errors of the system.');
        $p->setPower(30);
        $p->setInternalName(ArRole::NOTIFIED_FOR_ERRORS);
        $p->save();
        $roleId = $p->getId();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_RECEIVE_EMAILS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_REPORTS);
        $p->setArRoleId($roleId);
        $p->save();

        // new role

        $p = new ArRole();
        $p->setName('Notified for warning errors');
        $p->setDescription('Receive reports about warning errors of the system.');
        $p->setPower(30);
        $p->setInternalName(ArRole::NOTIFIED_FOR_WARNINGS);
        $p->save();
        $roleId = $p->getId();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_RECEIVE_EMAILS);
        $p->setArRoleId($roleId);
        $p->save();

        $p = new ArRoleHasPermission();
        $p->setArPermissionId(ArPermission::CAN_VIEW_REPORTS);
        $p->setArRoleId($roleId);
        $p->save();

        //////////////////////////////////////
        // Create Default Organization Type //
        //////////////////////////////////////

        $p = new ArOrganizationUnitType();
        $p->setName("Extension");
        $p->setShortCode('');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION);
        $p->save();

        $p = new ArOrganizationUnitType();
        $p->setName("External Telephone Number");
        $p->setShortCode('');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_EXTERNAL_TEL_NUMBER);
        $p->save();

        $p = new ArOrganizationUnitType();
        $p->setName("Root");
        $p->setShortCode('');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_ROOT);
        $p->save();

        $p = new ArOrganizationUnitType();
        $p->setName("Org");
        $p->setShortCode('');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_GENERIC_ORG);
        $p->save();

        $p = new ArOrganizationUnitType();
        $p->setName("System");
        $p->setShortCode('system');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_SYSTEM);
        $p->save();


        $p = new ArOrganizationUnitType();
        $p->setName("Customer");
        $p->setShortCode('');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_CUSTOMER);
        $p->save();


        $p = new ArOrganizationUnitType();
        $p->setName("Office");
        $p->setShortCode('');
        $p->setInternalName(ArOrganizationUnitType::ID_FOR_OFFICE);
        $p->save();

        //////////////////////////////////
        // CREATE DEFAULT CDR PROVIDERS //
        //////////////////////////////////

        $r = new ArCdrProvider();
        $r->setInternalName(ArCdrProvider::MANUAL_IMPORTING);
        $r->setDescription('Used for importing CDRs from ad-hoc files, created for fixing errors, or adding info during initial importing of data.');
        $r->save();

        $r = new ArCdrProvider();
        $r->setInternalName(ArCdrProvider::DEFAULT_SOURCE);
        $r->setDescription('The default source of CDRs.');
        $r->save();

        ////////////////////////////////////
        // CREATE DEFAULT RATE CATEGORIES //
        ////////////////////////////////////

        $r = new ArRateCategory();
        $r->setInternalName(ArRateCategory::ID_FOR_NORMAL);
        $r->save();

        ////////////////////////////
        // Create Type of Reports //
        ////////////////////////////

        $r = new ArReportOrderOfChildren();
        $r->setId(ArReportOrderOfChildren::ORDER_BY_NAME);
        $r->setName('order by organization name');
        $r->save();

        $r = new ArReportOrderOfChildren();
        $r->setId(ArReportOrderOfChildren::ORDER_BY_CALL_DURATION );
        $r->setName('order by calls duration');
        $r->save();

        $r = new ArReportOrderOfChildren();
        $r->setId(ArReportOrderOfChildren::ORDER_BY_CALL_COST );
        $r->setName('order by calls cost');
        $r->save();

        $r = new ArReportOrderOfChildren();
        $r->setId(ArReportOrderOfChildren::ORDER_BY_CALL_INCOME );
        $r->setName('order by calls income');
        $r->save();

        $r = new ArReportOrderOfChildren();
        $r->setId(ArReportOrderOfChildren::ORDER_BY_COUNT_OF_CALLS );
        $r->setName('order by calls count');
        $r->save();

        ///////////////////////////////////////
        // Create Type of Reports Generation //
        ///////////////////////////////////////

        $r = new ArReportGeneration();
        $r->setId(ArReportGeneration::GENERATE_ONLY_FOR_SPECIFIED_ORGANIZATION);
        $r->setName('Generate a unique cumulative report');
        $r->save();

        $r = new ArReportGeneration();
        $r->setId(ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS);
        $r->setName('Generate a distinct report for each billable organization');
        $r->save();

        $r = new ArReportGeneration();
        $r->setId(ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_THAT_ARE_NOT_VOIP_ACCOUNTS);
        $r->setName('Generate a distinct report for each main and children organization, but not extensions');
        $r->save();

        $r = new ArReportGeneration();
        $r->setId(ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_AND_VOIP_ACCOUNTS);
        $r->setName('Generate a distinct report for each main and children organization and extension');
        $r->save();

        $r = new ArReportGeneration();
        $r->setId(ArReportGeneration::GENERATE_FOR_ALL_CHILDREN_ORGANIZATIONS_WITH_A_RESPONSIBLE);
        $r->setName('Generate a distinct report for each main and children organization with a responsible');
        $r->save();

        ////////////////////////////////
        // Complete Init of Root User //
        ////////////////////////////////

        $root = ArUserPeer::getRootUser();
        if (!is_null($root)) {
            $r = new ArUserHasRole();
            $r->setArUserId($root->getId());
            $r->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
            $r->save();

            $r = new ArUserHasRole();
            $r->setArUserId($root->getId());
            $r->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::NOTIFIED_FOR_CRITICAL_ERRORS)->getId());
            $r->save();

            $r = new ArUserHasRole();
            $r->setArUserId($root->getId());
            $r->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::NOTIFIED_FOR_ERRORS)->getId());
            $r->save();
        }

        return $paramsId;
    }

    /**
     * Create default params.
     *
     * @return int defaultParamsId created
     */
    public static function getOrCreateDefaultParams()
    {
        $c = new Criteria();
        $c->add(ArParamsPeer::IS_DEFAULT, TRUE);
        $params = ArParamsPeer::doSelectOne($c);

        if (is_null($params)) {

            $params = new ArParams();
            $params->setOfficialCalldate(self::getGlobalStartingDateForCDRProcessinng());
            $params->setIsDefault(TRUE);
            $params->setName("Default");
            $params->setServiceName("Asterisell");
            $params->setServiceProviderWebsite("http://voipinfo.example.com");
            $params->setLegalWebsite("http://www.example.com");
            $params->setServiceProviderEmail("info@example.com");
            $params->setLogoImage("asterisell.png");
            $params->setLogoImageInInvoices("asterisell.jpeg");
            $params->setSlogan("web application for showing, and billing VoIP calls.");
            $params->setFooter("<center>For info contact:<a href=\"mailto:info@example.com\">info@example.com</a></center>");
            $params->setUserMessage("");

            $params->setVatTaxPerc(from_php_decimal_to_db_decimal("20"));

            $params->setLegalName("ACME Example VoIP Corporation");
            $params->setVat("WRLD12345678909876");
            $params->setLegalAddress("Street By Example");
            $params->setLegalCity("Soliera");
            $params->setLegalZipcode("41019");
            $params->setLegalStateProvince("Modena");
            $params->setLegalCountry("Italy");
            $params->setLegalEmail("acme@example.com");
            $params->setLegalPhone("+0 000-0000");

            $params->setSenderNameOnInvoicingEmails("ACME Corporation");
            $params->setInvoicingEmailAddress("sales@acme.example.com");
            $params->save();
        }

        return $params->getId();
    }

    static public function configureLegalDateGenerationMethod() {
      foreach(ArLegalDateGenerationMethod::getConstNames() as $key => $name) {
          $r  = new ArLegalDateGenerationMethod();
          $r->setId($key);
          $r->setName($name);
          $r->setDescription($name);
          $r->save();
      }
    }
}

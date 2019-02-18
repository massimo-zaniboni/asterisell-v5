<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

class InstallDemoData extends InstallationService
{

    const default_cdrs_to_create = 30000;

    /**
     * @var int how many months of data to generate.
     * NOTE: if you change this value, update also the `asterisell_instances.py` corresponding value.
     */
    const MONTHS_TO_GENERATE = 6;

    /**
     * @var null|int
     */
    protected $cdrsToCreate = null;

    /**
     * Set with the number of CDRs to create.
     *
     * @param int $v
     */
    public function setCdrsToCreate($v) {
        $this->cdrsToCreate = $v;
    }

    ///////////////////
    // JOB INTERFACE //
    ///////////////////

    public function process() {
        if (is_null($this->cdrsToCreate)) {
            $this->cdrsToCreate = self::default_cdrs_to_create;
        }

        $this->initWithDemoData($this->cdrsToCreate);

        return '';
    }

    /**
     * Add demo data.
     *
     * @param int $recordsToAdd
     * @return int the default paramsId
     */
    protected function initWithDemoData($recordsToAdd)
    {
        $conn = Propel::getConnection();
        $conn->beginTransaction();

        $internalExtensions = array();

        try {

            $defaultParamsId = ArParamsPeer::getDefaultParamsId();

            echo "\nCreating default vendor and categories.";

            $r = new ArRateCategory();
            $r->setInternalName(ArRateCategory::ID_FOR_DISCOUNTED);
            $r->save();

            $r = new ArParty();
            $r->setName("Default Vendor");
            $r->setExternalCrmCode("");
            $r->save();
            $defaultVendorPartyId = $r->getId();

            $r = new ArVendor();
            $r->setInternalName(ArVendor::ID_FOR_DEFUAULT);
            $r->setArPartyId($defaultVendorPartyId);
            $r->save();

            echo "\nCreating Parties";

            $normalCategoryId = ArRateCategoryPeer::retrieveByInternalName(ArRateCategory::ID_FOR_NORMAL)->getId();
            $discountedCategoryId = ArRateCategoryPeer::retrieveByInternalName(ArRateCategory::ID_FOR_DISCOUNTED)->getId();
            $defaultVendorId = ArVendorPeer::retrieveByInternalName(ArVendor::ID_FOR_DEFUAULT)->getId();

            // Create complex organization A

            $csvData = <<<'CSV'
"Type";"SIGLA";"Complete Name";"Account Code";
"unknown";"";"Acme Corporation";"";
"unknown";"";"Production Department";"1";
"unknown";"";"Production";"1.2";
"unknown";"";"Quality and Assurance";"1.3";
"unknown";"";"Maintenance";"1.4";
"unknown";"";"Marketing Department";"3";
"unknown";"";"Promotion and Media";"3.1";
"unknown";"";"Public Relations";"3.2";
"unknown";"";"Market Analyst";"3.3";
"unknown";"";"Technical Department";"4";
"unknown";"";"Product Design";"4.1";
"unknown";"";"Customer Service";"4.2";
"unknown";"";"Human Resource Department";"5";
"unknown";"";"Training Group";"5.1";
"unknown";"";"Recruiting Team";"5.2";
"unknown";"";"Office Support";"5.3";
CSV;

            $createHiearchy = new ConfigureOrganizationHierarchy();
            $createHiearchy->createOrganizationHierarchyFromCSVData($csvData, 'A.');

            // Create complex organization B

            $csvData = <<<'CSV'
"Type";"SIGLA";"Complete Name";"Account Code";
"unknown";"";"Altaplan Inc.";"";
"unknown";"";"Management";"1";
"unknown";"";"Legal";"2";
"unknown";"";"Payroll";"3";
"unknown";"";"Software Development";"4";
"unknown";"";"Hardware Production";"5";
CSV;

            $createHiearchy = new ConfigureOrganizationHierarchy();
            $createHiearchy->createOrganizationHierarchyFromCSVData($csvData, 'B.');

            // Create many small organizations

            $smallOrganizations = 20;
            $rateCategory = ArRateCategoryPeer::retrieveByInternalName(ArRateCategory::ID_FOR_NORMAL);
            $unitType = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_CUSTOMER);
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();
            for ($i = 1; $i <= $smallOrganizations; $i++) {
              $name = 'Mario Bross ' . $i;
              $p = new ArParty();
              $p->setName($name);
              $p->setIsBillable(true);
              $p->setIsActive(true);
              $p->save();

              $extensionCode = 'mario-bross-' . $i;
              $unitId = $this->createOrganization(null, $unitType->getId(), $name, $rateCategory->getId(), $p->getId());
              $this->createExtension($unitId, $extensionCode, $extensionId);
              $internalExtensions[] = $extensionCode;
            }

            // Create Extensions


            $code = '2020';
            $this->createCompleteExtension('Adam Jill', $code, 'A.1.2');
            $internalExtensions[] = $code;

            $code = '2021';
            $this->createCompleteExtension('Lekha Rubert', $code, 'A.1.2');
            $internalExtensions[] = $code;

            $code = '3020';
            $this->createCompleteExtension('Benny Glenn', $code, 'A.1.3');
            $internalExtensions[] = $code;

            $code = '3021';
            $this->createCompleteExtension('Jill Crest', $code, 'A.1.3');
            $internalExtensions[] = $code;

            $code = '2022';
            $this->createCompleteExtension('Lewis David', $code, 'A.3.1');
            $internalExtensions[] = $code;

            $code = '2023';
            $this->createCompleteExtension('Carl Lewis', $code, 'A.3.1');
            $internalExtensions[] = $code;

            $code = '2024';
            $this->createCompleteExtension('Elton John', $code, 'A.3.1');
            $internalExtensions[] = $code;

            $code = '2030';
            $this->createCompleteExtension('Carlo Provetoni', $code, 'A.3.2');
            $internalExtensions[] = $code;

            $code = '2031';
            $this->createCompleteExtension('Frank Kant', $code, 'A.3.2');
            $internalExtensions[] = $code;

            $code = '2032';
            $this->createCompleteExtension('Andrew Hudson', $code, 'A.3.3');
            $internalExtensions[] = $code;

            //

            $code = '5031';
            $this->createCompleteExtension('Mike Slate', $code, 'A.4.1');
            $internalExtensions[] = $code;

            $code = '5032';
            $this->createCompleteExtension('Kriss Kross', $code, 'A.4.1');
            $internalExtensions[] = $code;

            $code = '5033';
            $this->createCompleteExtension('Dave Ally', $code, 'A.4.1');
            $internalExtensions[] = $code;

            $code = '5034';
            $this->createCompleteExtension('Martin Scorzese', $code, 'A.4.1');
            $internalExtensions[] = $code;

            $code = '5035';
            $this->createCompleteExtension('Steve Baltman', $code, 'A.4.2');
            $internalExtensions[] = $code;

            $code = '5036';
            $this->createCompleteExtension('Emily Watson', $code, 'A.4.2');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();

            //


            $code = '9101';
            $this->createCompleteExtension('Meg Ryan', $code, 'B.1');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();


            $code = '9102';
            $this->createCompleteExtension('Alec Baldwin', $code, 'B.1');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();


            $code = '9103';
            $this->createCompleteExtension('Edward Burns', $code, 'B.2');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();


            $code = '9104';
            $this->createCompleteExtension('Rosario Dawson', $code, 'B.2');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();


            $code = '9105';
            $this->createCompleteExtension('Dennis Farina', $code, 'B.2');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();


            $code = '9106';
            $this->createCompleteExtension('Carol Kane', $code, 'B.3');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();


            $code = '9107';
            $this->createCompleteExtension('Matt LeBlanc', $code, 'B.3');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();

            $code = '9108';
            $this->createCompleteExtension('Garry Oldman', $code, 'B.4');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();

            $code = '9109';
            $this->createCompleteExtension('Christian Bale', $code, 'B.5');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();

            //

            $code = '6031';
            $this->createCompleteExtension('Mike Oldfield', $code, 'A.5.1');
            $internalExtensions[] = $code;

            $code = '6032';
            $this->createCompleteExtension('Luis Bell', $code, 'A.5.1');
            $internalExtensions[] = $code;

            $code = '6033';
            $this->createCompleteExtension('Kathy Bates', $code, 'A.5.2');
            $internalExtensions[] = $code;

            $code = '6034';
            $this->createCompleteExtension('Judy Parfitt', $code, 'A.5.2');
            $internalExtensions[] = $code;

            $code = '6035';
            $this->createCompleteExtension('Ned Walker', $code, 'A.5.2');
            $internalExtensions[] = $code;

            $code = '6036';
            $this->createCompleteExtension('Patty Duke', $code, 'A.5.3');
            $internalExtensions[] = $code;
            $extensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();

            // Create Users with different access rights

            $this->createResponsibleUser('Jane Alexander', 'jane', 'jane', 'A.');
            $this->createResponsibleUser('Tom Hardy', 'tom', 'tom', 'A.1');
            $this->createResponsibleUser('Max Wall', 'max', 'max', 'A.5');

            $this->createResponsibleUser('Kate Capshaw', 'kate', 'kate', 'B.');
            $this->createResponsibleUser('James Olmos', 'james', 'james', 'B.1');
            $this->createResponsibleUser('Billy Barty', 'billy', 'billy', 'B.3');

            $params = ArParamsPeer::getDefaultParams();

            $params->setOfficialCalldate(self::getGlobalStartingDateForCDRProcessinng());
            $params->setHtmlNotesOnTheLoginForm('
This is a demo instance.

You can login using these user / password accounts:<ul>
<li>admin / admin : for administering the application (only if you are accessing the "/admin" URL)</li>
<li>jane / jane : responsible of Acme Corporation (only if you are accessing the "/" root URL)</li>
<li>tom / tom : responsible of Production Department of Acme Corporation (only if you are accessing the "/" root URL)</li>
<li>kate / kate : responsible of Acme Corporation (only if you are accessing the "/" root URL)</li>
<li>billy / billy : responsible of Payroll Department of Altaplan Inc. (only if you are accessing the "/" root URL)</li>
</ul>
        ');
            $params->save();

            // Create main admin of the application

            $user = ArUserPeer::retrieveByLogin('admin');
            if ($user == null) {
                $party = new ArParty();
                $party->setName('admin');
                $party->save();

                $user = new ArUser();
                $user->setArPartyId($party->getId());
                $user->setArOrganizationUnitId(null);
                $user->setLogin('admin');
            }
            $user->setIsEnabled(true);
            $user->setClearPassword('admin');
            $user->setIsRootAdmin(true);
            $user->save();

            $userRole = new ArUserHasRole();
            $userRole->setArUserId($user->getId());
            $userRole->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::ADMIN)->getId());
            $userRole->save();

            $userRole = new ArUserHasRole();
            $userRole->setArUserId($user->getId());
            $userRole->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::NOTIFIED_FOR_ERRORS)->getId());
            $userRole->save();

            $userRole = new ArUserHasRole();
            $userRole->setArUserId($user->getId());
            $userRole->setArRoleId(ArRolePeer::retrieveByInternalName(ArRole::NOTIFIED_FOR_CRITICAL_ERRORS)->getId());
            $userRole->save();


            // Add more data to the party of root organizations,
            // in order to generate better invoices

            foreach (array('A.', 'B.') as $rootAccountCode) {
                $rootOrganizationId = ArOrganizationUnitPeer::retrieveByInternalName($rootAccountCode)->getId();

                $c = new Criteria();
                $c->add(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, $rootOrganizationId);
                $structure = ArOrganizationUnitHasStructurePeer::doSelectOne($c);

                $r = $structure->getArParty();

                $r->setVat("WRLD1111113333");
                $r->setLegalAddress("Street Avenue, 1");
                $r->setLegalCity("Soliera");
                $r->setLegalZipcode("41019");
                $r->setLegalStateProvince("Modena");
                $r->setLegalCountry("Italy");
                $r->setEmail("alpha@example.com");
                $r->setIsBillable(true);
                $r->save();
            }

            // Vendors

            ArVendorPeer::doDelete(new Criteria());

            $p = new ArParty();
            $p->setName('VoIP Infinity');
            $p->save();

            $v = new ArVendor();
            $v->setArParty($p);
            $v->save();
            $voipVendorId = $v->getId();

            $vs = new ArVendorDomain();
            $vs->setArVendor($v);
            $vs->setDomain(ConfigureCommunicationChannels::SIP_FIXED_LINE_OPERATOR_CHANNEL);
            $vs->setIsPrefix(false);
            $vs->setFrom(strtotime('-1 years', self::getGlobalStartingDateForCDRProcessinng()));
            $vs->setArCommunicationChannelTypeId(ArCommunicationChannelTypePeer::retrieveByInternalName(ConfigureCommunicationChannels::SIP_FIXED_LINE_OPERATOR_CHANNEL)->getId());
            $vs->setTo(null);
            $vs->save();

            $vs = new ArVendorDomain();
            $vs->setArVendor($v);
            $vs->setDomain(ConfigureCommunicationChannels::SIP_MOBILE_OPERATOR_CHANNEL);
            $vs->setIsPrefix(false);
            $vs->setFrom(strtotime('-1 years', self::getGlobalStartingDateForCDRProcessinng()));
            $vs->setArCommunicationChannelTypeId(ArCommunicationChannelTypePeer::retrieveByInternalName(ConfigureCommunicationChannels::SIP_MOBILE_OPERATOR_CHANNEL)->getId());
            $vs->setTo(null);
            $vs->save();

            // Update also the activation dates of other communication channels because they must be active
            // for more time, due to service CDRS generation.

            $domains = ArVendorDomainPeer::doSelect(new Criteria());
            foreach($domains as $d) {
                /**
                 * @var ArVendorDomain $d
                 */
                $d->setFrom(strtotime('-1 years', self::getGlobalStartingDateForCDRProcessinng()));
                $d->save();
            }

            //
            // Services
            //

            // NOTE: init in the past, because the bundle rates requires that a rate is defined on all the timeframe.
            $startingDate = strtotime('-2 months', FixedJobProcessor::getGlobalStartingDateForCDRProcessinng());

            $s = new ArService();
            $s->setIsEnabled(true);
            $s->setVendorName('ADSL 100M');
            $s->setCustomerName('ADSL Professional Plus');
            $s->setIsAppliedOnlyOneTime(false);
            $s->setScheduleTimeframe('monthly');
            $s->setScheduleFrom('1');
            $s->save();

            $sp = new ArServicePrice();
            $sp->setArService($s);
            $sp->setPrice(100000);
            $sp->setFromDate(fromUnixTimestampToMySQLTimestamp($startingDate));
            $sp->save();

            $ss = new ArAssignedService();
            $ss->setFromDate($startingDate);
            $ss->setArService($s);
            $ss->setNrOfItems(1);
            $ss->setDiscount(0);
            $ss->setArOrganizationUnit(ArOrganizationUnitPeer::retrieveByInternalName('A.'));
            $ss->save();


            //
            // Rates
            //


            $rate = new ArRate();
            $rate->setFromTime($startingDate);
            $rate->setInternalName("csv-1");
            $rate->setArRateId(null);
            $rate->setArRateFormat(ArRateFormatPeer::retrieveByInternalName(InitRateFormats::REFERENCE_CSV_WITH_HEADER_3COL_IT));
            $rate->setSourceDataFileContentFromPlainText(file_get_contents(getAsterisellCompleteRootDirectory() . '/scripts/installation/international_call_rates_example.csv'));
            $rate->save();

            $costRateSpec = '
rate {
  id: main

  rate {
    id: default-outgoing
    match-call-direction: outgoing
    use: csv-1
    set-cost-for-minute: external
  }

  rate {
    id: free-incoming
    match-call-direction: incoming
  }

  rate {
    id: free-internal
    match-call-direction: internal
  }
}
        ';

            $incomeRateSpec = '
bundle {
  id: free-60-calls

  service-cdr-type: Bundle Rate
  service-cdr-description: 60 free monthly calls

  schedule: monthly
  schedule-from: 1
  apply-for-each: normal

  limits-are-proportionals-to-activation-date: true
  only-for-calls-with-a-cost: true

  bundle-cost: 10

  rate {
    id: bundle
    limit-on-first-calls: none
    limit-on-first-seconds: 3600
  }
}

rate {
  id: outgoing
  match-call-direction: outgoing
  use: csv-1
  set-cost-on-call: 0.1
  set-cost-for-minute: external
}

rate {
  id: internal
  match-call-direction: internal
  set-cost-on-call: 0
}

rate {
  id: incoming
  match-call-direction: incoming
  set-cost-on-call: 0
}


';

            $rateFormatId = ArRateFormatPeer::retrieveByInternalName(InitRateFormats::REFERENCE_RATE_PLAN_SPECIFICATION);

            $rate = new ArRate();
            $rate->setFromTime($startingDate);
            $rate->setInternalName(ArRatePeer::MAIN_COST_RATE);
            $rate->setArRateId(null);
            $rate->setArRateFormat($rateFormatId);
            $rate->setSourceDataFileContentFromPlainText($costRateSpec);
            $rate->save();

            $rate = new ArRate();
            $rate->setFromTime($startingDate);
            $rate->setInternalName(ArRatePeer::MAIN_INCOME_RATE);
            $rate->setArRateId(null);
            $rate->setArRateFormat($rateFormatId);
            $rate->setSourceDataFileContentFromPlainText($incomeRateSpec);
            $rate->save();

            // Add CDRs

            $externalTelephoneNumbers = array(
                "3905956565656",
                "3905194234234",
                "3932894234234",
                "3933394234234",
                "3944894234234",
                "545494234234234",
                "54544234234234",
                "54541111122333",
                "545491234333",
                "559872342333",
                "5599721234333",
                "559993423423423",
                "559993423423423",
                "55999342342423",
                "559993422368",
                "5599934239999",
                "17094234234234",
                "17077824234234",
                "17077824234234",
                "17077824666544",
                "17077824456453",
                "861893345345345",
                "861893345234234",
                "861893343333",
                "86189334534434",
                "8622242342344",
                "8623423442444",
                "86234234424234",
                "8623423442898",
                "8623423442978",
                "454288234234",
                "454288234234",
                "4542882344563",
                "45428823466467",
                "45428823426868",
                "454253811231234",
                "454253811231234",
                "4542538112334534",
                "33607345345345",
                "3360734534523423",
                "336073453453345",
                "33607345345666",
                "33607345348887",
                "3360734534598989",
                "33607345345385645",
                "33677222343434",
                "3367722555664",
                "33677222366546",
                "336772223466565");

            $fileName = ImportDataFiles::createAbsoluteInputDataFileName('initial_demo_data', ArCdrProvider::MANUAL_IMPORTING, 'asterisell-standard', 'v1');
            $handle = fopen($fileName, "w");

            $ar_channels = array(ConfigureCommunicationChannels::SIP_MOBILE_OPERATOR_CHANNEL, ConfigureCommunicationChannels::SIP_FIXED_LINE_OPERATOR_CHANNEL);
            $directions = array('incoming', 'outgoing');
            $minDate = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();
            $maxDate = strtotime('+' . self::MONTHS_TO_GENERATE . ' months', $minDate);
            $secondsRange = $maxDate - $minDate;

            for ($i = 1; $i <= $recordsToAdd; $i++) {
                if ($i % 1000 == 0) {
                    echo "\nAdded $i CDR demo records\n";
                }

                $secondsToAdd = intval(rand(0, $secondsRange));
                $date = $minDate + $secondsToAdd;
                $duration = intval(rand(0, 60 * 5));

                $data = array();
                $data[] = fromUnixTimestampToMySQLTimestamp($date);
                $data[] = '\N';
                $data[] = $this->my_array_rand($directions);
                $data[] = "false";
                $data[] = 'ANSWERED';
                $data[] = $duration;
                $data[] = $this->my_array_rand($internalExtensions);
                $data[] = $this->my_array_rand($externalTelephoneNumbers);
                $data[] = '';
                $data[] = $this->my_array_rand($ar_channels);
                $data[] = '';

                safe_fputcsv($handle, $data);
            }

            fclose($handle);
            echo "\nAdded $recordsToAdd CDR demo records to source data file.\n";

            $conn->commit();
        } catch (Exception $e) {
            $conn->rollBack();
            echo "Caught exception: " . $e->getMessage() . "\n";
            echo $e->getTraceAsString();
        }
    }
}
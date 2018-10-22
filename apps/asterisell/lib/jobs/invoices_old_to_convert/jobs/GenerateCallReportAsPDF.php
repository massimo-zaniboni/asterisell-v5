<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Read InvoiceDetailsData, calculate other queries and
 * generate a call-report as PDF.
 *
 * Store it in ar_invoice.pdf_call_report.
 */
class GenerateCallReportAsPDF extends JobProcessor {

  /** 
   * how many most-expensive/longest/etc.. calls to display
   */
  const CALLS_TO_DISPLAY = 30;

  public function processEvent(JobData $jobData, $parentId) {

    if (! ($jobData instanceof InvoiceDetailsData)) {
      return null;
    }

    $showOutgoingCalls = sfConfig::get('app_show_outgoing_calls');
    $showIncomingCalls = sfConfig::get('app_show_incoming_calls');

    // Retrieve data
    //
    $invoice = $jobData->getArInvoice();
    $party = $invoice->getArParty();
    $params = $party->getArParams();
    $limit = GenerateCallReportAsPDF::CALLS_TO_DISPLAY;

    // Work only on invoice to customers, that are no revenue sharing
    //
    if (! ($invoice->getType() == "C")) {
      return null;
    }

    if ($invoice->getIsRevenueSharing() == 1) {
      return null;
    }

    // Call report header
    //
    $pdf = new PDFCallReportTemplate($invoice);
    $pdf->AddPage();

    // Query for list of most something calls...
    //
    $c = new Criteria();
    $c2 = $c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrFrom(), Criteria::GREATER_EQUAL);
    $c2->addAnd($c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrTo(), Criteria::LESS_THAN));
    $c->add($c2);
    $c->clearSelectColumns();

    $c->addSelectColumn(ArOfficePeer::AR_PARTY_ID);
    $c->addSelectColumn(ArCdrPeer::ID);
    $c->addSelectColumn(ArCdrPeer::BILLSEC);
    $c->addSelectColumn(ArCdrPeer::INCOME);
    $c->addSelectColumn(ArCdrPeer::CACHED_MASKED_EXTERNAL_TELEPHONE_NUMBER);
    $c->addSelectColumn(ArCdrPeer::CALLDATE);
    $c->addSelectColumn(ArAsteriskAccountPeer::NAME);
    $c->addSelectColumn(ArOfficePeer::NAME);
    $c->addSelectColumn(ArTelephonePrefixPeer::NAME);
    $c->addSelectColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION);
    $c->addSelectColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);

    $c->addJoin(ArCdrPeer::AR_ASTERISK_ACCOUNT_ID, ArAsteriskAccountPeer::ID);
    $c->addJoin(ArAsteriskAccountPeer::AR_OFFICE_ID, ArOfficePeer::ID);

    $c->add(ArOfficePeer::AR_PARTY_ID, $party->getId());

    $c->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID);

    $c->setLimit($limit);

    if ($showOutgoingCalls) {
      // Display most expensive calls (outgoing)
      //
      $c2 = clone($c);
      $c2->addDescendingOrderByColumn(ArCdrPeer::INCOME);
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::outgoing);

      $this->addMostSomethingCalls($c2, $pdf, "$limit Most Expensive Outgoing Calls", $params, $invoice, $party);

      $pdf->AddPage();

      // Display longest calls (outgoing)
      //
      $c2 = clone($c);
      $c2->addDescendingOrderByColumn(ArCdrPeer::BILLSEC);
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::outgoing);

      $this->addMostSomethingCalls($c2, $pdf, "$limit Longest Duration Outgoing Calls", $params, $invoice, $party);

      $pdf->AddPage();
    }

    if ($showIncomingCalls) {
      // Display most expensive calls (incoming)
      //
      $c2 = clone($c);
      $c2->addAscendingOrderByColumn(ArCdrPeer::INCOME);
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::incoming);
      
      $this->addMostSomethingCalls($c2, $pdf, "$limit Most Expensive Incoming Calls", $params, $invoice, $party);

      $pdf->AddPage();
      
      // Display longest calls (incoming)
      //
      $c2 = clone($c);
      $c2->addDescendingOrderByColumn(ArCdrPeer::BILLSEC);
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::incoming);
      
      $this->addMostSomethingCalls($c2, $pdf, "$limit Longest Duration Incoming Calls", $params, $invoice, $party);
      $pdf->AddPage();

    }

    // Prepare query for grouping calls
    //
    $c = new Criteria();
    $c2 = $c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrFrom(), Criteria::GREATER_EQUAL);
    $c2->addAnd($c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrTo(), Criteria::LESS_THAN));
    $c->add($c2);
    $c->clearSelectColumns();

    $c->addJoin(ArCdrPeer::AR_ASTERISK_ACCOUNT_ID, ArAsteriskAccountPeer::ID);
    $c->addJoin(ArAsteriskAccountPeer::AR_OFFICE_ID, ArOfficePeer::ID);
    $c->add(ArOfficePeer::AR_PARTY_ID, $party->getId());

    $c->addGroupByColumn(ArCdrPeer::CACHED_EXTERNAL_TELEPHONE_NUMBER);

    $c->addSelectColumn('SUM(cdr.income) as my_income');
    $c->addSelectColumn('SUM(cdr.billsec)');
    $c->addSelectColumn('MAX(cdr.ar_telephone_prefix_id)'); 
    // NOTE: I'm using MAX instead of FIRST because MySQL uptodate has no FIRST...
    $c->addSelectColumn('MAX(cdr.cached_masked_external_telephone_number)');
    $c->addSelectColumn('COUNT(cdr.cached_external_telephone_number) as my_count');

    $c->setLimit($limit);

    if ($showOutgoingCalls) {
      // Display most frequently dialled numbers (outgoing)
      //
      $c2 = clone($c);
      $c2->addDescendingOrderByColumn('my_count');
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::outgoing);
      
      $this->addMostSomethingGroupedCalls($c2, $pdf, "$limit Most Frequently Dialled Numbers", $params, $invoice, $party);

      $pdf->AddPage();
      
      // Display most expensive dialled numbers (outgoing)
      //
      $c2 = clone($c);
      $c2->addDescendingOrderByColumn('my_income');
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::outgoing);
      
      $this->addMostSomethingGroupedCalls($c2, $pdf, "$limit Most Expensive Dialled Numbers", $params, $invoice, $party);

      $pdf->AddPage();
    }

    if ($showIncomingCalls) {
      // Display most frequently dialled numbers (incoming)
      //
      $c2 = clone($c);
      $c2->addDescendingOrderByColumn('my_count');
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::incoming);
      
      $this->addMostSomethingGroupedCalls($c2, $pdf, "$limit Most Frequently Calling Numbers", $params, $invoice, $party);

      $pdf->AddPage();
      
      // Display most expensive calling numbers (incoming)
      //
      $c2 = clone($c);
      $c2->addAscendingOrderByColumn('my_income');
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::incoming);
      
      $this->addMostSomethingGroupedCalls($c2, $pdf, "$limit Most Expensive Calling Numbers", $params, $invoice, $party);

      $pdf->AddPage();
    }

    // Query for listing call details grouped by destination
    // 
    $c = new Criteria();
    $c2 = $c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrFrom(), Criteria::GREATER_EQUAL);
    $c2->addAnd($c->getNewCriterion(ArCdrPeer::CALLDATE, $invoice->getArCdrTo(), Criteria::LESS_THAN));
    $c->add($c2);

    $c->addJoin(ArCdrPeer::AR_ASTERISK_ACCOUNT_ID, ArAsteriskAccountPeer::ID);
    $c->addJoin(ArAsteriskAccountPeer::AR_OFFICE_ID, ArOfficePeer::ID);
    $c->add(ArOfficePeer::AR_PARTY_ID, $party->getId());
    $c->addJoin(ArCdrPeer::AR_TELEPHONE_PREFIX_ID, ArTelephonePrefixPeer::ID);

    $c->addGroupByColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION);
    $c->addGroupByColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);
    $c->addGroupByColumn(ArTelephonePrefixPeer::NAME);

    $c->clearSelectColumns();
    $c->addSelectColumn('SUM(' . ArCdrPeer::COUNT_OF_CALLS . ') as nr_of_calls');
    $c->addSelectColumn('SUM(' . ArCdrPeer::BILLSEC . ')');
    $c->addSelectColumn('SUM(' . ArCdrPeer::INCOME . ')');
    $c->addSelectColumn(ArTelephonePrefixPeer::GEOGRAPHIC_LOCATION);
    $c->addSelectColumn(ArTelephonePrefixPeer::NAME);
    $c->addSelectColumn(ArTelephonePrefixPeer::OPERATOR_TYPE);

    $c->addDescendingOrderByColumn('nr_of_calls');

    if ($showOutgoingCalls) {
      // Display details about outgoing calls
      //
      $c2 = clone($c);
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::outgoing);
      $this->addDetailsAboutCalls($c2, $pdf, "Dialled Numbers Analysis", $params, $invoice, $party);
    }

    if ($showIncomingCalls) {
      $pdf->AddPage();

      // Display details about incoming calls
      //
      $c2 = clone($c);
      $c2->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::incoming);
      $this->addDetailsAboutCalls($c2, $pdf, "Calling Numbers Analysis", $params, $invoice, $party);
    }

    // Save the PDF report.
    //
    $pdfInvoice = $pdf->Output("", "S");
    $invoice->setPdfCallReport($pdfInvoice);
    $invoice->save();

    return '';
  }

  /**
   * Add informations about most something calls.
   */
  protected function addMostSomethingCalls($query, $pdf, $title, $params, $invoice, $party) {
    $index_party = 1;
    $index_count = 2;
    $index_billSec = 3;
    $index_income = 4;
    $index_telNr = 5;
    $index_callDate = 6;
    $index_accountName = 7;
    $index_officeName = 8;
    $index_prefixName = 9;
    $index_prefixLocation = 10;
    $index_prefixType = 11;

    $colsNames=array(
		     mytr("Telephone Number"),
		     mytr("Destination"),
		     mytr("Date"),
		     mytr("Duration"),
		     mytr("Cost"),
		     mytr("User"));
    
    $colsDim = array(35,48,30,20,15,38);

    $colsNamesAlign=array("C", "C", "C", "R", "R", "R"); 

    $colsAlign     =array("L", "L", "R", "R", "R", "R");

    $rs = BasePeer::doSelect($query);

    $lines = array();
    while ($rs->next()) {
      $line = array($rs->get($index_telNr),
                    ArTelephonePrefix::calcDescriptiveName($rs->get($index_prefixType), $rs->get($index_prefixLocation), $rs->get($index_prefixName)),
		    format_date_according_config($rs->get($index_callDate)),
		    format_minute($rs->get($index_billSec)),
		    from_db_decimal_to_pdf_txt_decimal($rs->get($index_income)),
		    $rs->get($index_accountName) . " / " . $rs->get($index_officeName));

      $lines[] = $line; 
    }

    $pdf->addCallTable($title, $colsNames, $colsNamesAlign, $colsAlign, $colsDim, $lines);
    
  }

  /**
   * Add informations about most something calls.
   */
  protected function addMostSomethingGroupedCalls($query, $pdf, $title, $params, $invoice, $party) {
    $index_sumIncome = 1;
    $index_sumBillsec = 2;
    $index_telephonePrefixId = 3;
    $index_maskedNumber = 4;
    $index_countCalls = 5;

    $colsNames=array(
		     mytr("Telephone Number"),
		     mytr("Destination"),
		     mytr("Number of Calls"),
		     mytr("Total Duration"),
		     mytr("Total Cost"));
    
    $colsDim = array(35,55,30,25,25);

    $colsNamesAlign=array("C", "C", "L", "L", "L");

    $colsAlign=array("L", "L", "L", "L", "L");

    $rs = BasePeer::doSelect($query);

    $lines = array();
    while ($rs->next()) {
      $prefix = ArTelephonePrefixPeer::retrieveByPk($rs->get($index_telephonePrefixId));

      $line = array($rs->get($index_maskedNumber),
                    $prefix->getDescriptiveName(),
                    $rs->get($index_countCalls),
		    format_minute($rs->get($index_sumBillsec)),
		    from_db_decimal_to_pdf_txt_decimal($rs->get($index_sumIncome)));

      $lines[] = $line; 
    }

    $pdf->addCallTable($title, $colsNames, $colsNamesAlign, $colsAlign, $colsDim, $lines);
    
  }


  /**
   * Add details information about calls.
   */
  protected function addDetailsAboutCalls($query, $pdf, $title, $params, $invoice, $party) {

    $index_count = 1;
    $index_billSec = 2;
    $index_income = 3;
    $index_prefixLocation = 4;
    $index_prefixName = 5;
    $index_prefixType = 6;

    $colsNames=array(
		     mytr("Destination"),
		     mytr("Number of Calls"),
		     mytr("Total Duration"),
		     mytr("Total Cost"));
    
    $colsDim = array(50,35,35,35);

    $colsNamesAlign = array("L", "L", "L", "L");
    $colsAlign=array("L", "L", "L", "L");

    $rs = BasePeer::doSelect($query);

    $lines = array();
    while ($rs->next()) {

      $line = array(ArTelephonePrefix::calcDescriptiveName($rs->get($index_prefixType), $rs->get($index_prefixLocation), $rs->get($index_prefixName)),
                    $rs->get($index_count),
		    format_minute($rs->get($index_billSec)),
		    from_db_decimal_to_pdf_txt_decimal($rs->get($index_income)));

      $lines[] = $line; 
    }

    $pdf->addCallTable($title, $colsNames, $colsNamesAlign, $colsAlign, $colsDim, $lines);
    
  }


}
?>
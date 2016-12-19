<?php
  /**************************************************************
   !!!                                                        !!!
   !!! WARNING: This file is automatic generated.             !!!
   !!!                                                        !!!
   !!! In order to modify this file change the content of     !!!
   !!!                                                        !!!
   !!!    /module_template/invoice_template                   !!!
   !!!                                                        !!!
   !!! and execute                                            !!!
   !!!                                                        !!!
   !!!    sh generate_modules.sh                              !!!
   !!!                                                        !!!
   **************************************************************/

class customer_invoiceActions extends autoCustomer_invoiceActions {

  /**
   * Force the type of Invoice to be equal to the type declarated for the module.
   */
  protected function saveArInvoice($ar_invoice) {
    $ar_invoice->setType('C');
    return parent::saveArInvoice($ar_invoice);
  }

  /**
   * Force a filter on the type of invoice
   */
  protected function addFiltersCriteria($c) {
    $c->add(ArInvoicePeer::TYPE, 'C');
    return parent::addFiltersCriteria($c);
  }

  /**
   * Manage "my_ar_party_id" custom selector.
   */
  protected function updateArInvoiceFromRequest()
  {
    // Handle the input of the partial field
    $partyId = $this->getRequestParameter('my_ar_party_id');
    $this->ar_invoice->setArPartyId($partyId);

    $this->ar_invoice->setInfoOrAdsImage1($this->getRequestParameter('my_info_image1_file'));
    $this->ar_invoice->setInfoOrAdsImage2($this->getRequestParameter('my_info_image2_file'));

    parent::updateArInvoiceFromRequest();
  }

  public function executeEdit() {

    if ($this->getRequestParameter('show_pdf_report')) {
      $this->ar_invoice = $this->getArInvoiceOrCreate();
      $id = $this->ar_invoice->getId();
              return $this->redirect('customer_invoice/showPDFReport?id=' . $id);
          }

    if ($this->getRequestParameter('show_pdf_call_report')) {
      $this->ar_invoice = $this->getArInvoiceOrCreate();
      $id = $this->ar_invoice->getId();
              return $this->redirect('customer_invoice/showPDFCallReport?id=' . $id);
          }

    // TRUE if this request was intercepted from this method
    // and need the update of job queue.
    //
    $interceptedRequet = FALSE;

    if ($this->getRequestParameter('regenerate_invoice')) {
      $interceptedRequet = TRUE;
      $this->ar_invoice = $this->getArInvoiceOrCreate();
      $this->updateArInvoiceFromRequest();
      $this->saveArInvoice($this->ar_invoice);

      $id = $this->ar_invoice->getId();

      $d = new InvoiceData();
      $d->arPartyId = $this->ar_invoice->getArPartyId();
      $d->nr = $this->ar_invoice->getNr();
      $d->invoiceDate = $this->ar_invoice->getInvoiceDate();
      $d->cdrsFromDate = $this->ar_invoice->getArCdrFrom();
      $d->cdrsToDate = $this->ar_invoice->getArCdrTo();
      $d->isRevenueSharing = $this->ar_invoice->getIsRevenueSharing();
      $d->infoOrAdsImage1 = $this->ar_invoice->getInfoOrAdsImage1();
      $d->infoOrAdsImage2 = $this->ar_invoice->getInfoOrAdsImage2();
      $d->arParamsId = $this->ar_invoice->getArParamsId();

              $d->type = 'C';
      
      ArJobQueue::addNew($d, NULL);
    }
  
    
      // mails can be sent only to customer invoces
      //
    if ($this->getRequestParameter('send_email_to_customer')) {
      $interceptedRequet = TRUE;
      $this->ar_invoice = $this->getArInvoiceOrCreate();
      $this->updateArInvoiceFromRequest();
      $this->ar_invoice->setDisplayedOnline(TRUE);
      $this->saveArInvoice($this->ar_invoice);

      $id = $this->ar_invoice->getId();

      $d = new SendInvoiceEmailData();
      $d->arInvoiceId = $id;
      ArJobQueue::addNew($d, NULL);
    }
        
    if ($interceptedRequet) {
      $id = $this->ar_invoice->getId();

	  $this->setFlash('notice', __('Invoice will be generated at next execution of cron processor.'));

      	return $this->redirect('customer_invoice/edit?id=' . $id);
          } else {
      parent::executeEdit();
    }
  }

  public function executeShowPDFReport() {
    $this->ar_invoice = $this->getArInvoiceOrCreate();

    // NOTE: see templates/showPDFReportSuccess for real action
    // associated to this method...
  }

  public function executeShowPDFCallReport() {
    $this->ar_invoice = $this->getArInvoiceOrCreate();

    // NOTE: see templates/showPDFCallReportSuccess for real action
    // associated to this method...
  }

}

?>

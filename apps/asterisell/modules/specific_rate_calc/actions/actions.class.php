<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2020 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

class specific_rate_calcActions extends autoSpecific_rate_calcActions
{
 
    public function executeDownload_base_rate_diffs()
    {
        $id = $this->getRequest()->getParameter('id');
        if (!is_null($id)) {
          $d = ArSpecificRateCalcPeer::retrieveByPK($id);
          if (!is_null($d)) {
            $this->setLayout(false);
            sfConfig::set('sf_web_debug', false);

            $fileName = $d->getSpecificRateName();
            if (isEmptyOrNull($fileName)) {
              $fileName = "rate-" . $id;
            }

            // Adding the file to the Response object
            $this->getResponse()->clearHttpHeaders();
            $this->getResponse()->setHttpHeader('Pragma: private', true);
            $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="' . $fileName . '.csv"');
            $this->getResponse()->setContentType('text/plain; charset=utf-8');
            $this->getResponse()->sendHttpHeaders();
            $this->getResponse()->setContent($d->getMediumtextBaseRateDiffContent());

            return sfView::NONE;
          }
        }

        $this->forward('specific_rate_calc', 'list');
    }

    public function executeDownload()
    {
        $id = $this->getRequest()->getParameter('id');
        if (!is_null($id)) {
          $d = ArSpecificRateCalcPeer::retrieveByPK($id);
          if (!is_null($d)) {
            $this->setLayout(false);
            sfConfig::set('sf_web_debug', false);

            $fileName = $d->getSpecificRateName();
            if (isEmptyOrNull($fileName)) {
              $fileName = "rate-" . $id;
            }

            // Adding the file to the Response object
            $this->getResponse()->clearHttpHeaders();
            $this->getResponse()->setHttpHeader('Pragma: private', true);
            $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="' . $fileName . '.csv"');
            $this->getResponse()->setContentType('text/plain; charset=utf-8');
            $this->getResponse()->sendHttpHeaders();
            $this->getResponse()->setContent($d->getMediumtextSpecificRateOutContent());

            return sfView::NONE;
          }
        }

        $this->forward('specific_rate_calc', 'list');
    }

    public function updateArSpecificRateCalcFromRequest()
    {
        $rateId = $this->getRequestParameter('select_base_rate');
        if (!isEmptyOrNull($rateId)) {
            $this->ar_specific_rate_calc->setArRateId($rateId);
        } else {
            $this->ar_specific_rate_calc->setArRateId(null);
        }

        parent::updateArSpecificRateCalcFromRequest();
    }
    
  protected function getArSpecificRateCalcOrCreate($id = 'id')
  {
    if ($this->getRequestParameter($id) === ''
     || $this->getRequestParameter($id) === null)
    {
      $ar_specific_rate_calc = new ArSpecificRateCalc();
      $ar_specific_rate_calc->setMediumtextSpecificRateInMatchAll('description,operator,prefix,cost_by_minute,cost_on_call');
      $ar_specific_rate_calc->setMediumtextSpecificRateInMatchExact('description,operator,prefix,cost_by_minute,cost_on_call');
      
      // because MySQL does not support default values on blobs
      $ar_specific_rate_calc->setMediumtextBaseRateDiff('');
      $ar_specific_rate_calc->setMediumtextSpecificRateOut('');
      $ar_specific_rate_calc->setRatePlanOut('');
      $ar_specific_rate_calc->setCalcInfo('');
      $ar_specific_rate_calc->setCalcError('');
      $ar_specific_rate_calc->setMediumtextSpecificRateOut('');
      
      return $ar_specific_rate_calc;
    }
    else
    {
      return parent::getArSpecificRateCalcOrCreate($id);
    }
  }
  
  public function executeEdit($request)
  {
    if ($request->isMethod(sfRequest::POST)) {
      $this->ar_specific_rate_calc = $this->getArSpecificRateCalcOrCreate();
      $this->updateArSpecificRateCalcFromRequest();

      if ($this->getRequestParameter('calc_action')) {
        /**
         * @var ArSpecficRateCalc $rateCalc
         */
        $rateCalc = $this->ar_specific_rate_calc;
        $rateCalc->save();
        $rateCalcId = $rateCalc->getId();
        // I need to save it, because I need an Id to pass to RateEngine
        
        if (ArSpecificRateCalcPeer::calcSpecificRate($rateCalcId)) {
          $this->getUser()->setFlash('notice', 'Rate was generated');
        } else {
          $this->getUser()->setFlash('error', $rateCalc->getCalcError());
        }

        ArSpecificRateCalcPeer::clearInstancePool();
        $this->ar_specific_rate_calc = $this->getArSpecificRateCalcOrCreate($rateCalcId);
        
        return $this->redirect('specific_rate_calc/edit?id=' . $rateCalc->getId());
      }
    }

    return parent::executeEdit($request);
  }
}

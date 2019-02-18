<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Number', 'Form', 'Asterisell'));

class rateActions extends autoRateActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // use always this order, for being sure to modify the correct rate
        $c->addDescendingOrderByColumn(ArRatePeer::FROM_TIME);

        // force a sort on ID for viewing all the calls in the LIMIT pagination
        $c->addAscendingOrderByColumn(ArRatePeer::ID);
    }

    public function executeEdit($request)
    {
        //
        // Set current rate
        //

        $rateIdToClone = null;
        $this->ar_rate = null;
        if ($request->isMethod(sfRequest::POST)) {
            if ($this->getRequestParameter('add_rate')) {
                // this info will be used later, after saving of current rating, for propagating shared rates info
                $rateIdToClone = $this->getRequest()->getParameter('id');

                // in this case it is always a new rate, because I'm adding info to the history.
                $this->ar_rate = new ArRate();
            }
        }

        if (is_null($this->ar_rate)) {
            // in all other cases use default initialization.
            $this->ar_rate = $this->getArRateOrCreate();
        }

        // The rate will be changed, so it must be exported again
        $this->ar_rate->setIsExportedToResellers(false);

        //
        // Process Request.
        //

        if ($request->isMethod(sfRequest::POST)) {

            $this->updateArRateFromRequest();
            $errorMessage = null;

            $fileName = $this->getRequest()->getFilePath('csvFile');

            if (!is_null($fileName) && (strlen(trim($fileName)) > 0)) {
                $handle = fopen($fileName, 'r');
                if ($handle == false) {
                    $errorMessage = "Error opening file \"$fileName\"";
                } else {
                    $content = stream_get_contents($handle);
                    $this->ar_rate->setSourceDataFileContentFromPlainText($content);
                    $this->ar_rate->setHTMLDescriptionInPlainText('');
                    fclose($handle);
                }
            }

            $this->labels = $this->getLabels();

            try {
                $this->saveArRate($this->ar_rate);
                $newRateId = $this->ar_rate->getId();

                if (!isEmptyOrNull($newRateId) && !isEmptyOrNull($rateIdToClone)) {

                    $query = '
                    INSERT
                    INTO ar_rate_shared_with_reseller(ar_rate_id, ar_reseller_id, is_exported)
                    SELECT ?, o.ar_reseller_id, 0
                    FROM ar_rate_shared_with_reseller AS o
                    WHERE o.ar_rate_id = ?
                    ';

                    $conn = Propel::getConnection();
                    $stmt = $conn->prepare($query);
                    $stmt->execute(array($newRateId, $rateIdToClone));
                    $stmt->closeCursor();
                }

            } catch (PropelException $e) {
                $this->setErrorMessage('Could not save the edited rates.');
                return $this->forward('rate', 'list');
            }

            $id = $this->ar_rate->getId();
            $errorRedirect = 'rate/edit?id=' . $id;

            if (is_null($errorMessage)) {
                $this->setInfoMessage('Saved.');
            } else {
                $this->setErrorMessage("Error processing source data file.");
                return $this->redirect($errorRedirect);
            }
        } else {
            parent::executeEdit($request);
        }
    }

    /**
     * Add an error message.
     * @param string $msg
     */
    public function setErrorMessage($msg)
    {
        $this->getUser()->setFlash('error', "Rate was not saved. " . $msg);
    }

    /**
     * Add an error message.
     * @param string $msg
     */
    public function setInfoMessage($msg)
    {
        $this->getUser()->setFlash('notice', $msg);
    }

    public function executeDownload()
    {
        $id = $this->getRequest()->getParameter('id');
        return $this->performDownload(false, $id);
    }

    public function executeDownloadbackup()
    {
        $id = $this->getRequest()->getParameter('id');
        return $this->performDownload(true, $id);
    }

    protected function performDownload($isBackup, $id)
    {
        if (!is_null($id)) {
            $rate = ArRatePeer::retrieveByPK($id);

            if (!is_null($rate)) {
                if (!$isBackup) {
                    $content = $rate->getSourceDataFileContentInPlainText();
                } else {
                    $content = $rate->getBackupSourceDataFileContentInPlainText();
                }

                if (!is_null($content)) {
                    $this->setLayout(false);
                    sfConfig::set('sf_web_debug', false);
                    $this->getResponse()->clearHttpHeaders();
                    $this->getResponse()->setHttpHeader('Pragma: private', true);
                    $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="source_data_for_rate_' . $id . '.txt"');
                    $this->getResponse()->setContentType('text/plain; charset=utf-8');
                    $this->getResponse()->sendHttpHeaders();
                    $this->getResponse()->setContent($content);

                    return sfView::NONE;
                } else {
                    $this->redirect('rate/edit?id=' . $id);
                }
            }
        }

        return sfView::SUCCESS;
    }

    public function updateArRateFromRequest()
    {
        $formatId = $this->getRequestParameter('select_rate_format');
        if (!isEmptyOrNull($formatId)) {
            $this->ar_rate->setArRateFormatId($formatId);
        } else {
            $this->ar_rate->setArRateFormatId(null);
        }

        $vendorId = $this->getRequestParameter('select_vendor');
        if (isEmptyOrNull($vendorId)) {
            $vendorId = null;
        }
        $this->ar_rate->setArVendorId($vendorId);

        parent::updateArRateFromRequest();
    }


    protected function addFiltersCriteria($c)
    {

       if (isset($this->filters['filter_on_name_list']) && !isEmptyOrNull($this->filters['filter_on_name_list'])) {
         $c->add(ArRatePeer::INTERNAL_NAME, $this->filters['filter_on_name_list'], Criteria::LIKE);
       }
       parent::addFiltersCriteria($c);
    }
}

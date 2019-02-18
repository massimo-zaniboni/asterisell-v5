<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * telephone_prefix actions.
 *
 * @package    asterisell
 * @subpackage telephone_prefix
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 2692 2006-11-15 21:03:55Z fabien $
 */
class telephone_prefixActions extends autotelephone_prefixActions {

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArTelephonePrefixPeer::ID);
    }

    public function executeImportCSV($request)
    {
        $fileName = $request->getFilePath('csvFile');

        if (is_null($fileName) || (strlen(trim($fileName)) == 0)) {
            $this->getUser()->setFlash('error', "There is no CSV file.");
            $this->redirect('telephone_prefix/list');
        }

        $handle = fopen($fileName, 'r');
        if ($handle == false) {
            $this->getUser()->setFlash('error', "Error opening file \"$fileName\"");
            $this->redirect('telephone_prefix/list');
        }

        if ($this->getRequestParameter('csv')) {
            $useCSV = TRUE;
        } else {
            $this->getUser()->setFlash('error', "Unknowns command.");
            $this->redirect('telephone_prefix/list');
        }

        $isOk = true;
        $errorMessage = '';
        $processor = new LoadWorldTelephonePrefixesFromCSVFile();
        try {
            $count = $processor->loadPrefixesFromHandle($handle, true);
        } catch(Exception $e) {
            $isOk = false;
            $errorMessage = $e->getMessage();
        }

        if ($isOk) {
            $this->getUser()->setFlash('notice', "Inserted $count new telephone prefixes.");
        } else {
            $this->getUser()->setFlash('error', "Error during importing of prefixes. " . ArProblemException::getLastErrorDescription());
        }

        $this->redirect('telephone_prefix/list');
    }
}

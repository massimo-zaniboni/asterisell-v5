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

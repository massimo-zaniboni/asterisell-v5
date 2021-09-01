<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2021 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

class set_hacked_callsActions extends autoSet_hacked_callsActions
{
    /**
     * Executes index action
     */
    public function executeIndex($request)
    {
        $this->forward('default', 'module');
    }

    public function executeImportCSV($request)
    {
        $fileName = $request->getFilePath('csvFile');

        if (is_null($fileName) || (strlen(trim($fileName)) == 0)) {
            $this->getUser()->setFlash('error', "There is no CSV file.");
            $this->redirect('set_hacked_calls/list');
        }

        $handle = fopen($fileName, 'r');
        if ($handle == false) {
            $this->getUser()->setFlash('error', "Error opening file \"$fileName\"");
            $this->redirect('set_hacked_calls/list');
        }

        if ($this->getRequestParameter('csv')) {
          $processor = new HackedCalls();
          $processOk = $processor->executeImportCSV($handle, $useCSV);

          if (is_string($processOk)) {
            $this->getUser()->setFlash('error', $processOk);
          } else {
            $this->getUser()->setFlash('notice', HackedCalls::showResult($processOk));
          }

          $this->redirect('set_hacked_calls/list');
         } else {
            $this->getUser()->setFlash('error', "Unknowns command.");
            $this->redirect('set_hacked_calls/list');
        }
   }
}

<?php

/**
 * params actions.
 *
 * @package    asterisell
 * @subpackage params
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 2692 2006-11-15 21:03:55Z fabien $
 */
class number_portabilityActions extends autonumber_portabilityActions
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
            $this->redirect('number_portability/list');
        }

        $handle = fopen($fileName, 'r');
        if ($handle == false) {
            $this->getUser()->setFlash('error', "Error opening file \"$fileName\"");
            $this->redirect('number_portability/list');
        }


        if ($this->getRequestParameter('csv')) {
            $useCSV = TRUE;
        } else if ($this->getRequestParameter('twt')) {
            $useCSV = FALSE;
        } else {
            $this->getUser()->setFlash('error', "Unknowns command.");
            $this->redirect('number_portability/list');
        }

        $processor = new NumberPortability();
        $processOk = $processor->executeImportCSV($handle, $useCSV);

        if (is_string($processOk)) {
            $this->getUser()->setFlash('error', $processOk);
        } else {
            $this->getUser()->setFlash('notice', "Inserted $processOk new telephone numbers.");
        }

        $this->redirect('number_portability/list');
    }
}

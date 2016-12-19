<?php

sfLoader::loadHelpers(array('Asterisell'));

class reportActions extends autoReportActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArReportPeer::ID);
    }

    public function executeEdit($request)
    {
        if ($request->isMethod(sfRequest::POST)) {
            if (is_null($this->ArReport)) {
                // in all other cases use default initialization.
                $this->ArReport = $this->getArReportOrCreate();
            }

            $this->updateArReportFromRequest();

            $phpClassName = $this->getRequestParameter('select_premade_report_generator');
            if (!isEmptyOrNull($phpClassName)) {
                $this->ArReport->setPhpClassName($phpClassName);
            }

            try {
                $this->saveArReport($this->ArReport);
            } catch (PropelException $e) {
                $request->setError('edit', 'Could not save.');
                return $this->forward('report', 'list');
            }

            if ($this->getRequestParameter('generate_report')) {
                $prof = new JobProfiler('report');
                $conn = Propel::getConnection();
                $conn->beginTransaction();
                try {
                   $this->ArReport->generateDocument($conn);
                   $conn->commit();
                } catch (ArProblemException $e) {
                    $conn->rollBack();
                    $this->getUser()->setFlash('error', $e::getLastErrorDescription());
                    return $this->redirect('report/edit?id=' . $this->ArReport->getId());
                }

                $prof->incrementProcessedUnits();
                $this->getUser()->setFlash('notice', 'Your modifications have been saved. ' . $prof->stop());
                return $this->redirect('report/edit?id=' . $this->ArReport->getId());
            }

            if ($this->getRequestParameter('confirm_report')) {
                try {
                    ArReportPeer::publishReportToUsers($this->ArReport->getId(), true, false);
                } catch (ArProblemException $e) {
                    $this->getUser()->setFlash('error', $e::getLastErrorDescription());
                    return $this->redirect('report/edit?id=' . $this->ArReport->getId());
                }

                $this->getUser()->setFlash('notice', 'Your modifications have been saved');
                return $this->redirect('report/edit?id=' . $this->ArReport->getId());
            }

            // manage normal form saving
            return parent::executeEdit($request);

        } else {
          return parent::executeEdit($request);
        }
    }

    public function updateArReportFromRequest()
    {
        parent::updateArReportFromRequest();

        $organizationId = $this->getRequestParameter('select_voip_account');
        if (isEmptyOrNull($organizationId)) {
            $organizationId = $this->getRequestParameter('select_proper_organization');
        }

        if (!isEmptyOrNull($organizationId)) {
            $this->ArReport->setArOrganizationUnitId($organizationId);
        } else {
            $this->ArReport->setArOrganizationUnitId(null);
        }

        $vendorId = $this->getRequestParameter('select_vendor');
        if (!isEmptyOrNull($vendorId)) {
            $this->ArReport->setArVendorId($vendorId);
        } else {
            $this->ArReport->setArVendorId(null);
        }
    }

    public function executeDownload()
    {
        $id = $this->getRequest()->getParameter('id');
        if (!is_null($id)) {
            $report = ArReportPeer::retrieveByPK($id);
            if (!is_null($report)) {
                if ($report->isThereDocument()) {
                    $this->setLayout(false);
                    sfConfig::set('sf_web_debug', false);

                    $filename = $report->getProducedReportFileName();
                    if (isEmptyOrNull($filename)) {
                        $filename = 'report';
                    }

                    // Adding the file to the Response object
                    $this->getResponse()->clearHttpHeaders();
                    $this->getResponse()->setHttpHeader('Pragma: private', true);
                    $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="' . $filename . '"');
                    $this->getResponse()->setContentType($report->getProducedReportMimeType() . '; charset=utf-8');
                    $this->getResponse()->sendHttpHeaders();
                    $this->getResponse()->setContent($report->getDocumentContent());

                    return sfView::NONE;
                }
            }
        }

        $this->forward('report', 'list');
    }

    protected function addFiltersCriteria($c)
    {

        $showOnlyToReview = $this->getRequestParameter(ArReportSet::SHOW_ONLY_TO_REVIEW_PARAM_NAME);

        if (!is_null($showOnlyToReview)) {

            $c->add(ArReportPeer::PRODUCED_REPORT_ALREADY_REVIEWED, false);
            $c->add(ArReportPeer::AR_REPORT_SET_ID, null);
            $c->add(ArReportPeer::IS_TEMPLATE, false);

        } else {

            if (isset($this->filters['filter_on_organization']) && !isEmptyOrNull($this->filters['filter_on_organization'])) {
                $c->add(ArReportPeer::AR_ORGANIZATION_UNIT_ID, $this->filters['filter_on_organization']);
            }
        }

        parent::addFiltersCriteria($c);
    }

}

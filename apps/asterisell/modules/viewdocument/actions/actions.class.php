<?php

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

class viewdocumentActions extends autoviewdocumentActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArReportToReadUserViewPeer::ID);
    }

    /**
     * Force a filter for the correct type of user.
     */
    protected function addFiltersCriteria($c)
    {
        parent::addFiltersCriteria($c);

        /**
         * @var AsterisellUser $user
         */
        $user = sfContext::getInstance()->getUser();
        $userId = $user->getUserId();

        $c->add(ArReportToReadUserViewPeer::AR_USER_ID, $userId);
    }

    /**
     * Download the document.
     *
     * @return string
     */
    public function executeEdit($request)
    {

        $id = $request->getParameter('id');

        if (!is_null($id)) {

            // NOTE: the id of the view is the same of the ArReportToRead
            $read = ArReportToReadPeer::retrieveByPK($id);

            /**
             * @var AsterisellUser $user
             */
            $user = sfContext::getInstance()->getUser();

            if ((!is_null($read)) && $read->getArUserId() == $user->getUserId()) {
                // the user can read the document

                $read->setSeenOrReceivedFromUser(true);
                $read->save();

                $reportId = $read->getArReportId();

                if (!is_null($reportId)) {
                    $report = ArReportPeer::retrieveByPK($reportId);

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
            }

        }

        $this->redirect('viewdocument/list');

        // $this->forward('viewdocument', 'list');
    }

    public function executeShowcallreport($request)
    {
        /**
         * @var AsterisellUser $user
         */
        $user = sfContext::getInstance()->getUser();
        $this->redirect($user->getCallReportModuleName());
    }

    /**
     * Avoid access.
     *
     * @param string $id
     * @return null
     */
    protected function getArReportToReadUserViewOrCreate($id = 'id')
    {
        return null;
    }

    /**
     * Force a make nothing policy.
     */
    protected function saveArReportToReadUserView($ar_document)
    {
    }

    /**
     * Force a make nothing policy.
     */
    protected function deleteArReportToReadUserView($ar_document)
    {
    }

    /**
     * Force a make nothing policy.
     */
    protected function updateArReportToReadUserViewFromRequest()
    {
    }

    /**
     * Do nothing.
     *
     * @param $request
     */
    public function executeCreate($request)
    {
    }

    /**
     * Do nothing
     * @param $request
     */
    public function executeDeleteSelected($request)
    {

    }
}


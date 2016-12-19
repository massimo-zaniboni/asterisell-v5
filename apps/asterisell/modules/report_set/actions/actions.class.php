<?php


sfLoader::loadHelpers(array('Asterisell'));

/**
 * report_set actions.
 *
 * @package    asterisell
 * @subpackage report_set
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class report_setActions extends autoReport_setActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArReportSetPeer::ID);
    }

    public function executeEdit($request)
    {
        if ($request->isMethod(sfRequest::POST)) {
            $this->ar_report_set = $this->getArReportSetOrCreate();
            $this->updateArReportSetFromRequest();

            /**
             * @var ArReportSet $reportSet
             */
            $reportSet = $this->ar_report_set;
            $reportSetId = $reportSet->getId();

            if ($this->getRequestParameter('generate_report')) {
                $reportScheduler = $reportSet->getArReportScheduler();

                if (!is_null($reportScheduler)) {
                    $referenceDate = fromMySQLTimestampToUnixTimestamp($reportSet->getFromDate());

                    $d = new ForceExecutionOfReportScheduler();
                    $d->arReportSchedulerId = $reportSet->getArReportSchedulerId();
                    $d->fromDate = $referenceDate;
                    ArJobQueuePeer::addNew($d, null, null);

                    $log = 'Report generation job is added to the queue. Reports are not generated online, but they will be generated at next execution passage of the job scheduler.';
                    $this->getUser()->setFlash('notice', $log);
                    return $this->redirect('report_set/edit?id=' . $reportSet->getId());
                }
            }

            if ($this->getRequestParameter('delete_all_reports')) {
                try {
                    ArReportSetPeer::deleteAssociatedReports($reportSetId, Propel::getConnection());
                } catch (ArProblemException $e) {
                    $this->getUser()->setFlash('error', $e::getLastErrorDescription());
                    return $this->redirect('report_set/edit?id=' . $reportSet->getId());
                }
            }

            if ($this->getRequestParameter('confirm_all_reports')) {
                $conn = Propel::getConnection();
                $conn->beginTransaction();
                try {
                    ArReportSetPeer::confirmAssociatedReports($reportSetId, $conn);
                    $conn->commit();
                } catch (ArProblemException $e) {
                    $conn->rollBack();
                    $this->getUser()->setFlash('error', $e::getLastErrorDescription());
                    return $this->redirect('report_set/edit?id=' . $reportSet->getId());
                }
            }

            $this->getUser()->setFlash('notice', 'Your modifications have been saved');
            return $this->redirect('report_set/edit?id=' . $reportSet->getId());

        } else {
            return parent::executeEdit($request);
        }
    }

    protected function addFiltersCriteria($c)
    {

        $showOnlyToReview = $this->getRequestParameter(ArReportSet::SHOW_ONLY_TO_REVIEW_PARAM_NAME);

        if (!is_null($showOnlyToReview)) {
            $c->add(ArReportSetPeer::MUST_BE_REVIEWED, true);
        }

        parent::addFiltersCriteria($c);
    }

}

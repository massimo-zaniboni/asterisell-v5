<?php

sfLoader::loadHelpers(array('Asterisell'));

/**
 * report_scheduling actions.
 *
 * @package    asterisell
 * @subpackage report_scheduling
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class report_schedulingActions extends autoReport_schedulingActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArReportSchedulerPeer::ID);
    }

    public function executeEdit($request)
    {
        if ($request->isMethod(sfRequest::POST)) {
            $this->ArReportScheduler = $this->getArReportSchedulerOrCreate();
            $this->updateArReportSchedulerFromRequest();

            if (is_null($this->ArReportScheduler->getArReportGenerationId())) {
                $this->ArReportScheduler->setArReportGenerationId(ArReportGeneration::GENERATE_FOR_ALL_BILLABLE_CHILDREN_ORGANIZATIONS);
            }

            try {
                $this->saveArReportScheduler($this->ArReportScheduler);
            } catch (PropelException $e) {
                $this->getUser()->setFlash('error', 'Could not save: ' . $e->getMessage());
                return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
            }

            $log = '';

            if ($this->getRequestParameter('generate_report')) {

                $generator = new ScheduledReportGenerator();
                $generator->setArReportScheduler($this->ArReportScheduler);
                try {
                    $generator->checkParams();
                }  catch(ArProblemException $e) {
                    $this->getUser()->setFlash('error', ArProblemException::getLastErrorDescription());
                    return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
                }

                $fromDate = $this->ArReportScheduler->getLastFromDate();
                if (!is_null($fromDate)) {
                    $fromDate = fromMySQLTimestampToUnixTimestamp($fromDate);

                    $d = new ForceExecutionOfReportScheduler();
                    $d->arReportSchedulerId = $this->ArReportScheduler->getId();
                    $d->fromDate = $fromDate;

                    ArJobQueuePeer::addNew($d, null, null);

                    $log = 'Report generation job is added to the queue. Reports are not generated online, but they will be generated at next execution passage of the job scheduler.';
                } else {
                    $this->getUser()->setFlash('error', "Report field \"From Date\" is required.");
                    return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
                }
            }

            $generator = new ScheduledReportGenerator();
            $generator->setArReportScheduler($this->ArReportScheduler);

            if ($this->getRequestParameter('delete_all_reports')) {
                try {
                    $fromDate = $this->ArReportScheduler->getLastFromDate();
                    if (is_null($fromDate)) {
                        $this->getUser()->setFlash('error', "Report field \"From Date\" is required.");
                        return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
                    } else {
                        $fromDate = fromMySQLTimestampToUnixTimestamp($fromDate);
                        $generator->deleteAssociatedReports($this->ArReportScheduler, $fromDate);
                    }
                } catch (ArProblemException $e) {
                    $this->getUser()->setFlash('error', $e::getLastErrorDescription());
                    return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
                }
            }

            if ($this->getRequestParameter('confirm_all_reports')) {
                try {
                    $fromDate = $this->ArReportScheduler->getLastFromDate();
                    if (is_null($fromDate)) {
                        $this->getUser()->setFlash('error', "Report field \"From Date\" is required.");
                        return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
                    } else {
                        $fromDate = fromMySQLTimestampToUnixTimestamp($fromDate);
                        $generator->confirmAssociatedReports($this->ArReportScheduler, $fromDate);
                    }
                } catch (ArProblemException $e) {
                    $this->getUser()->setFlash('error', $e::getLastErrorDescription());
                    return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());
                }
            }

            $this->getUser()->setFlash('notice', 'Your modifications have been saved. ' . $log);
            return $this->redirect('report_scheduling/edit?id=' . $this->ArReportScheduler->getId());

        } else {
            return parent::executeEdit($request);
        }
    }


    protected function addFiltersCriteria($c)
    {

        if (isset($this->filters['filter_on_organization']) && !isEmptyOrNull($this->filters['filter_on_organization'])) {
            $c->add(ArReportSchedulerPeer::AR_ORGANIZATION_UNIT_ID, $this->filters['filter_on_organization']);
        }
        parent::addFiltersCriteria($c);
    }

    public function updateArReportSchedulerFromRequest()
    {
        parent::updateArReportSchedulerFromRequest();

        // Remove the time from the selector.
        // NOTE: I'm not using a date selector without time, because there is a bug in the widget
        $date = $this->ArReportScheduler->getLastFromDate();
        if (!is_null($date)) {
            $date = fromMySQLTimestampToUnixTimestamp($date);

            $this->ArReportScheduler->setLastFromDate(startWith00Timestamp($date));
        }

        $methodId = $this->getRequestParameter('select_legal_date_generation_method');
        if (isEmptyOrNull($methodId)) {
            $methodId = null;
        }
        $this->ArReportScheduler->setArLegalDateGenerationMethodId($methodId);

        $templateId = $this->getRequestParameter('select_report_template');
        if (!isEmptyOrNull($templateId)) {
            $this->ArReportScheduler->setArReportId($templateId);
        } else {
            $this->ArReportScheduler->setArReportId(null);
        }

        $organizationId = $this->getRequestParameter('select_voip_account');
        if (isEmptyOrNull($organizationId)) {
            $organizationId = $this->getRequestParameter('select_proper_organization');
        }

        if (!isEmptyOrNull($organizationId)) {
            $this->ArReportScheduler->setArOrganizationUnitId($organizationId);
        } else {
            $this->ArReportScheduler->setArOrganizationUnitId(null);
        }

        $value = $this->getRequestParameter('insert_minimum_amount');
        if (!isEmptyOrNull($value)) {
            $value = convertToDbMoney($value);
        }
        $this->ArReportScheduler->setMinimumCost($value);

    }
}


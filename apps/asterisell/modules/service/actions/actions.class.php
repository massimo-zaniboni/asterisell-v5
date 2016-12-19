<?php

sfLoader::loadHelpers(array('Asterisell'));

/**
 * service actions.
 *
 * @package    asterisell
 * @subpackage service
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class serviceActions extends autoServiceActions
{
    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArServicePeer::ID);
    }


    protected function updateArServiceFromRequest()
    {
        $id = $this->getRequestParameter('select_schedule_from');
        if (isEmptyOrNull($id)) {
            $id = null;
        }
        $this->ar_service->setScheduleFrom($id);

        $id = $this->getRequestParameter('select_scheduling');
        if (isEmptyOrNull($id)) {
            $id = null;
        }
        $this->ar_service->setScheduleTimeframe($id);

        parent::updateArServiceFromRequest();
    }
}

<?php

/**
 * service_sale actions.
 *
 * @package    asterisell
 * @subpackage service_sale
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class service_saleActions extends autoService_saleActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArAssignedServicePeer::ID);
    }

    protected function updateArAssignedServiceFromRequest()
    {
        $unitId = $this->getRequestParameter('select_organization');
        $this->ar_assigned_service->setArOrganizationUnitId($unitId);

        $serviceId = $this->getRequestParameter('select_service');
        $this->ar_assigned_service->setArServiceId($serviceId);

        parent::updateArAssignedServiceFromRequest();
    }

    protected function addFiltersCriteria($c)
    {
        if (isset($this->filters['filter_on_organization'])) {
            $c->add(ArAssignedServicePeer::AR_ORGANIZATION_UNIT_ID, $this->filters['filter_on_organization']);
        }

        parent::addFiltersCriteria($c);
    }

}

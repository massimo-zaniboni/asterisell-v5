<?php

/**
 * vendor_domain actions.
 *
 * @package    asterisell
 * @subpackage vendor_domain
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class vendor_domainActions extends autoVendor_domainActions
{
    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArVendorDomainPeer::ID);
    }

}

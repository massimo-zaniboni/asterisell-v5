<?php

/**
 * cdr_provider actions.
 *
 * @package    asterisell
 * @subpackage cdr_provider
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class cdr_providerActions extends autoCdr_providerActions
{
    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArCdrProviderPeer::ID);
    }

}

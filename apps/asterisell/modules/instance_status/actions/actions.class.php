<?php

/**
 * instance_status actions.
 *
 * @package    asterisell
 * @subpackage instance_status
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class instance_statusActions extends autoInstance_statusActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArInstanceStatusPeer::ID);
    }

}

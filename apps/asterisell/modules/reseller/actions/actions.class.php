<?php

class resellerActions extends autoResellerActions
{

    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArResellerPeer::ID);
    }

}

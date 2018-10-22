<?php

// SPDX-License-Identifier: GPL-3.0-or-later
sfLoader::loadHelpers(array('Asterisell'));

class partyActions extends autopartyActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArPartyPeer::ID);
    }

    protected function updateArPartyFromRequest()
    {
        $limit30_str = $this->getRequestParameter('insert_money_value');
        $limit30 = convertToDbMoney($limit30_str);
        $this->ar_party->setMaxLimit30($limit30);
        parent::updateArPartyFromRequest();
    }
}

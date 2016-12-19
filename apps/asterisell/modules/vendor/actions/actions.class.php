<?php

/**
 * vendor actions.
 *
 * @package    asterisell
 * @subpackage vendor
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class vendorActions extends autoVendorActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArVendorPeer::ID);
    }


    public function updateArVendorFromRequest()
    {
        $partyId = $this->getRequestParameter('select_party');
        if (isEmptyOrNull($partyId)) {
            $partyId = null;
        } else if ($partyId == VariableFrame::CREATE_NEW_PARTY_ID) {
            $party = new ArParty();
            $party->setName('New party to configure');
            $party->save();
            $partyId = $party->getId();
        }
        $this->ar_vendor->setArPartyId($partyId);

        parent::updateArVendorFromRequest();
    }
}

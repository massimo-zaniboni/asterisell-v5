<?php

class fiscal_documentActions extends autoFiscal_documentActions
{

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArReportPeer::ID);
    }

    /**
     * Select only legal documents.
     */
    protected function addFiltersCriteria($c)
    {
        parent::addFiltersCriteria($c);
        $c->add(ArReportPeer::LEGAL_CONSECUTIVE_NR, 0, Criteria::GREATER_EQUAL);
    }

}

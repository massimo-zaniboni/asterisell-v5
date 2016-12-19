<?php

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

class root_organizationsActions extends autoRoot_organizationsActions
{

    public function executeCreate($request)
    {
        $o = new ArOrganizationUnit();
        $o->save();

        $p = new ArParty();
        $p->setName("New created customer " . $o->getId());
        $p->setIsBillable(true);
        $p->setIsActive(true);
        $p->save();

        $s = new ArOrganizationUnitHasStructure();
        $s->setArRateCategoryId(ArRateCategoryPeer::retrieveIdByInternalName(ArRateCategory::ID_FOR_NORMAL));
        $s->setArOrganizationUnitId($o->getId());
        $s->setFrom(FixedJobProcessor::getGlobalStartingDateForCDRProcessinng());
        $s->setExists(true);
        $s->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_GENERIC_ORG)->getId());
        $s->setArPartyId($p->getId());
        $s->save();

        return $this->redirect('organization_full_view/view?id=' . $o->getId());
    }

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArPartyPeer::NAME);
        $c->addAscendingOrderByColumn(ArOrganizationUnitHasStructurePeer::ID);
    }

    /**
     * Select all the organizations having no parent at some timestamp.
     * They are all the root organization of the past, present and future.
     *
     * @param Criteria $c
     */
    protected function addFiltersCriteria($c) {
        $c->addJoin(ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID, ArOrganizationUnitPeer::ID, Criteria::INNER_JOIN);
        $c->addJoin(ArOrganizationUnitHasStructurePeer::AR_PARTY_ID, ArPartyPeer::ID, Criteria::INNER_JOIN);

        if (isset($this->filters['filter_on_party_name']) && !isEmptyOrNull($this->filters['filter_on_party_name'])) {
            $s = $this->filters['filter_on_party_name'];
            $sl = strlen($s);
            if (substr($s, $sl - 1, 1) == '*') {
               $f = substr($s, 0, $sl - 1) . '%';
            } else {
                $f = $s;
            }

            $c->add(ArPartyPeer::NAME, $f, Criteria::LIKE);
        }

        // search for a structure with a null parent, now, in the past or in the future.
        // In this way an user can select all the root organizations of future and past.
        $c->add(ArOrganizationUnitHasStructurePeer::AR_PARENT_ORGANIZATION_UNIT_ID, null, Criteria::ISNULL);
        $c->setDistinct();

    }
}

<?php

sfLoader::loadHelpers(array('I18N', 'Debug', 'Url', 'Asterisell', 'Form'));

class userActions extends autoUserActions
{
    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArUserPeer::ID);
    }

    protected function updateArUserFromRequest()
    {
        parent::updateArUserFromRequest();

        $userId = $this->getRequest()->getParameter('id');

        if ($this->getRequestParameter('disable_login')) {
            $this->ar_user->setPassword(null);
            $password = null;
        } else {
            $password = $this->getRequest()->getParameter('newpassword');
            if (isEmptyOrNull($password)) {
                // does nothing, leaving previous password
            } else {
                if (!isValidStrForSQLQuery($password)) {
                    $this->getUser()->setFlash('error', 'Password contains invalid characters.');
                    $this->abortEdit($userId);
                } else {
                    $this->ar_user->setClearPassword($password);
                }
            }
        }

        $organizationId = $this->getRequestParameter('select_organization');
        if (!isEmptyOrNull($organizationId)) {
            $this->ar_user->setArOrganizationUnitId($organizationId);
        } else {
            $this->ar_user->setArOrganizationUnitId(null);
        }

        $partyId = $this->getRequestParameter('select_party');
        if (isEmptyOrNull($partyId) || $partyId == VariableFrame::USE_ORGANIZATION_PARTY_ID) {
            $partyId = null;
            // by default associate a user to the party associated to the organization
            if (!isEmptyOrNull($organizationId)) {
                $partyId = OrganizationUnitInfo::getInstance()->getArPartyId($organizationId, null);
            }
        } else if ($partyId == VariableFrame::CREATE_NEW_PARTY_ID) {
            $party = new ArParty();
            $party->setName('New party to configure');
            $party->save();
            $partyId = $party->getId();
        }
        $this->ar_user->setArPartyId($partyId);
    }

    /**
     * @param int|null $id
     */
    protected
    function abortEdit($id)
    {

        if (is_null($id)) {
            $this->redirect('user/list');
        } else {
            $this->redirect('user/edit?id=' . $id);
        }
    }

    public
    function executeEdit($request)
    {
        if ($request->isMethod(sfRequest::POST)) {

            $this->ar_user = $this->getArUserOrCreate();
            $this->updateArUserFromRequest();

            $login = $this->ar_user->getLogin();
            if (!isEmptyOrNull($login)) {
                if (!isValidStrForSQLQuery($login)) {
                    $this->getUser()->setFlash('error', 'Login contains invalid characters.');
                    $this->abortEdit($this->ar_user->getId());
                } else {
                    $duplicated = ArUserPeer::retrieveByLogin($login);
                    if (!is_null($duplicated) && $duplicated->getId() != $this->ar_user->getId()) {
                        $this->getUser()->setFlash('error', 'Login name must be unique.');
                        $this->abortEdit($this->ar_user->getId());
                    }
                }
            }
        }

        return parent::executeEdit($request);
    }

    protected
    function addFiltersCriteria($c)
    {

        if (isset($this->filters['filter_on_organization']) && !isEmptyOrNull($this->filters['filter_on_organization'])) {
            $c->add(ArUserPeer::AR_ORGANIZATION_UNIT_ID, $this->filters['filter_on_organization']);
        }
        parent::addFiltersCriteria($c);
    }
}

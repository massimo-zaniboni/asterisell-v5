<?php

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

class cdrlist_unprocessedActions extends autocdrlist_unprocessedActions {

  /**
   * Override addSortCriteris in order to add a more strict filter.
   *
   * @param Criteria $c
   */
  protected function addFiltersCriteria($c) {
    $c->add(ArCdrPeer::DESTINATION_TYPE, DestinationType::error);
  }

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArCdrPeer::ID);
    }

    protected function manageSecurityError() {
        return $this->redirect('cdrlist_unprocessed/list');
    }

    public function executeCreate($request)
    {
        return $this->manageSecurityError();
    }

    public function executeSave($request)
    {
        return $this->manageSecurityError();
    }

    public function executeEdit($request)
    {
        return $this->manageSecurityError();
    }

    public function executeDelete($request)
    {
        return $this->manageSecurityError();
    }

    public function handleErrorEdit()
    {
        return $this->manageSecurityError();
    }

    /**
     * Do nothing
     * @param $request
     */
    public function executeDeleteSelected($request) {

    }


    /**
     * Make explicitely nothing
     */
    protected function saveArCdr($cdr)
    {

    }

    /**
     * Make explicitely nothing
     */
    protected function deleteArCdr($cdr)
    {

    }

}

<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

class problemActions extends autoproblemActions {

    /**
     * Hide the TYPE_INTERNAL_LOG info from the user.
     *
     * POSTCONDITION: the resulting $c does not contain any select field
     * (required from the pager that adds its fields)
     *
     * @param Criteria $c
     */
     protected function addFiltersCriteria($c) {
         $c->add(ArCurrentProblemPeer::AR_PROBLEM_TYPE_ID, ArProblemType::TYPE_INTERNAL_LOG, Criteria::NOT_EQUAL);

         // NOTE: in case the condition is overridden by most specific filters
         parent::addFiltersCriteria($c);
     }

    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArCurrentProblemPeer::DUPLICATION_KEY);
    }

    public function executeDeleteProblems() {
        FixedJobProcessor::setCleanErrorTable(null, 1);
        return $this->forward('problem', 'list');
    }

  public function executeRefreshView() {
    return $this->redirect('problem/list');
  }

  public function executeSeeJobQueue() {
    return $this->redirect('jobqueue/list');
  }

  public function executeSearchproblem($request) {
      $key = $request->getParameter('problemkey');

      if (! isEmptyOrNull($key)) {
          $c = new Criteria();
          $c->add(ArCurrentProblemPeer::DUPLICATION_KEY, $key);
          $p = ArCurrentProblemPeer::doSelectOne($c);

          if (! is_null($p)) {
              $this->redirect('problem/edit?id=' . $p->getId());
          }
      }

      $this->redirect('problem/list');
  }

  public function executeExportToFile($request) {
        // execute list operation and then invoke templates/exportToFileSuccess.php
        //
        return $this->executeList($request);
  }
}

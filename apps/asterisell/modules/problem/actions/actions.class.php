<?php

/* $LICENSE 2009, 2010:
 *
 * Copyright (C) 2009, 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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
}

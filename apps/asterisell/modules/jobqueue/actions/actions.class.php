<?php

/**
 * jobqueue actions.
 *
 * @package    asterisell
 * @subpackage jobqueue
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 2692 2006-11-15 21:03:55Z fabien $
 */
class jobqueueActions extends autojobqueueActions
{
  /**
   * Executes index action
   *
   */
  public function executeIndex($request)
  {
    $this->forward('default', 'module');
  }


  public function executeRefreshView($request) {
    return $this->redirect('jobqueue/list');
  }

  public function executeSeeProblems($request) {
    return $this->redirect('problem/list');
  }
}

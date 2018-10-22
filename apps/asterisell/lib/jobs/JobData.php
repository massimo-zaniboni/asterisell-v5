<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Contains the info used from a Job to performs its work.
 */
abstract class JobData {

  /**
   * @return string a user readable description of the job requested from the Data.
   */
  abstract public function getDescription();

}

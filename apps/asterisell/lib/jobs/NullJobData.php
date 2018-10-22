<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class NullJobData extends JobData  {

  protected $d = "";

  public function setDescription($v) {
    $this->d = $v;
  }

  public function getDescription() {
    return $this->d;
  }

}

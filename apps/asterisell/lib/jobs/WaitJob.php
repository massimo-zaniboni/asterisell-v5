<?php

// SPDX-License-Identifier: GPL-3.0-or-later


/**
 * Make Nothing, waiting 45 seconds, 
 * in order to test locks and other aspects of the system.
 */
class WaitJob extends FixedJobProcessor {

  public function process() {
    sleep(45);
    return TRUE;
  }
}
?>
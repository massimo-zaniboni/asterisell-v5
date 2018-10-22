<?php

// SPDX-License-Identifier: GPL-3.0-or-later


/**
 * Make Nothing.
 */
class VoidJob extends JobProcessor {

  public function processEvent(JobData $jobData, $parentId) {
    return '';
  }
}
?>
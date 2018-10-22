<?php

// SPDX-License-Identifier: GPL-3.0-or-later


/**
 * Make Nothing.
 */
class NullJob extends FixedJobProcessor {

  public function process() {
    return '';
  }
}
?>
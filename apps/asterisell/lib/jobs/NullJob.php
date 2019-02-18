<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>


/**
 * Make Nothing.
 */
class NullJob extends FixedJobProcessor {

  public function process() {
    return '';
  }
}
?>
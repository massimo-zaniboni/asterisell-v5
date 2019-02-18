<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Url', 'Asterisell', 'Form', 'Asset'));

/**
 * Show and edit all the info about a hierarchical organization.
 */
class get_extension_listActions extends sfActions
{
    public function executeExportToCsv($request) {
        // execute list operation and then invoke templates/exportToCsvSuccess.php
        //
        return $this->executeList($request);
    }

    public function executeList($request) {
        // do nothing
    }
}

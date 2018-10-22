<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Url', 'Asterisell', 'Form', 'Asset'));

/**
 * Show and edit all the info about a hierarchical organization.
 */
class get_type_of_customersActions extends sfActions
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

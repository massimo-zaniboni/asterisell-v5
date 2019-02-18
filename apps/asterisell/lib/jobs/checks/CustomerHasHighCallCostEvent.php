<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * The Customer has high call costs.
 */
class CustomerHasHighCallCostEvent extends JobData
{

    public $unitId = NULL;
    public $arPartyId = NULL;
    public $effectiveCost = NULL;
    public $costLimit = NULL;
    public $fromDate = NULL;
    public $toDate = NULL;
    public $method = "";

    public function getDescription()
    {
        return __("The user/unit with id \"" . $this->unitId . "\" has high call costs.");
    }
}

?>
<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class UpdateWholesaleNumberStats extends FixedJobProcessor
{
    public function process()
    {
        self::updateStats(true);
        return '';
    }

    /**
     * Update the stats.
     * @param boolean $rethrowErrors true for rethrowing transaction problems
     * @return void
     * @throws Exception
     */
    static public function updateStats($rethrowErrors = false)
    {
        try {
            ChangeWholesaleInfo::updateWholesaleInfo();
        } catch (Exception $e) {
            if ($rethrowErrors) {
                throw($e);
            }
        }
    }
}

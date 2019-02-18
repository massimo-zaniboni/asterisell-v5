<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * After initial configuration, force a rerating without waiting an initial cron job processor.
 */
class ForceReratingAtCurrentCronJobProcessor extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        self::setWaitForScheduledRerate(false);
        return '';
    }
}

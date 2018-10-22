<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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

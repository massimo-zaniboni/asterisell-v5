<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class BackupOrganizationInfoEvent extends JobData
{

    const INTERNAL_NAME_IN_JOB_QUEUE = "BackupOrganizationInfo";

    public function getDescription()
    {
        return self::INTERNAL_NAME_IN_JOB_QUEUE;
    }
}

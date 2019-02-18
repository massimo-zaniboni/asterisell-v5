<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CDRs from external Asterisell instances, using RSYNC.
 */
abstract class RSyncCDRsForAggregateServer extends ImportCSVFilesFromRemoteServer
{

    ////////////////////////////
    // INTERFACE TO CUSTOMIZE //
    ////////////////////////////

    public function getLocalRSyncDirectory() {
        return getInstanceConfigValue('local_rsync_with_remote_servers');
    }

    public function getInstanceSettingsName() {
        return 'external_csv_servers';
    }
}
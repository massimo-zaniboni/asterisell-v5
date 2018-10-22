<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Remove old items from the various caches of the system.
 */
class CleanCacheFromOldItems extends FixedJobProcessor
{

    const EXECUTION_INTERVAL_IN_MINUTES = 60;

    public function process()
    {

        $timeFrameInMinutes = self::EXECUTION_INTERVAL_IN_MINUTES;
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {

            // Remove old job log items.
            // NOTE: surprisingly these logs use a lot of space (~300MB for 6 months of usage),
            // because they are a lot,
            // and also because I store some meta-info inside them.
            // So I take care to delete some info here.

            $oldMonths = sfConfig::get('app_months_after_removing_a_job_log_entry');
            $oldDate = strtotime("-$oldMonths month");

            // Delete old jobs
            $connection = Propel::getConnection();
            $stmt = $connection->prepare('DELETE FROM ar_job_queue WHERE created_at < ?');
            $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($oldDate)));

            // Delete meta-info, in recently executed jobs
            $connection = Propel::getConnection();
            $stmt = $connection->prepare('
                UPDATE ar_job_queue
                SET php_data_job_serialization = NULL
                WHERE created_at > ?
                AND   state = 2');

            $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($oldDate)));
        }
        return '';
    }
}

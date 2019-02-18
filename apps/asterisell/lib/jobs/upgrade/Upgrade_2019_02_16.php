<?php

class Upgrade_2019_02_16 extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function isDBUpgradeJob()
    {
        return true;
    }

    public function process()
    {
        $conn = Propel::getConnection();

        $conn->exec('ALTER TABLE `ar_cached_grouped_cdr` ADD COLUMN `count_of_records` BIGINT  NOT NULL');
        RateEngineService::executeUpdateAllCachedCDRS();

        return '';
    }
}

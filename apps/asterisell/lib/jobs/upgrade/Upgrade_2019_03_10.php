<?php

class Upgrade_2019_03_10 extends AdminJobProcessor
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
        $conn->exec('ALTER TABLE ar_organization_unit ADD COLUMN internal_checksum5 VARCHAR(200)');

        return '';
    }
}

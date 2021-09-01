<?php

class Upgrade_2020_07_24 extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return true;
    }

    public function isDBUpgradeJob()
    {
        return true;
    }

    public function process()
    {
        $conn = Propel::getConnection();
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN imported_info VARCHAR(4096)');
        return '';
   }
}

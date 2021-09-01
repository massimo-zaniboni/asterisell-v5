<?php

class Upgrade_2020_08_09 extends AdminJobProcessor
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
        
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN exported_internal_telephone_number VARCHAR(1024)  NOT NULL');
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN exported_billable_customer_ar_party_id INTEGER');
        
        return '';
   }
}

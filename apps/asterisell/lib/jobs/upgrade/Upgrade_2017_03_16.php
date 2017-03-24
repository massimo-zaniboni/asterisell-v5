<?php

class Upgrade_2017_03_16 extends AdminJobProcessor
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

        $conn->exec('ALTER TABLE ar_report_scheduler ADD COLUMN `send_compact_report_list_to_accountant` TINYINT default 0 NOT NULL;');
        $conn->exec('ALTER TABLE ar_report ADD COLUMN about_ar_report_set_id INTEGER');
        $conn->exec('ALTER TABLE ar_report
                     ADD CONSTRAINT `ar_report_foreign_about_report_set`
                     FOREIGN KEY (`about_ar_report_set_id`)
		             REFERENCES `ar_report_set` (`id`)
		             ON DELETE CASCADE');

        $conn->exec('CREATE INDEX index_on_ar_report_about ON ar_report(`about_ar_report_set_id`)');

        return '';
   }
}

<?php

class Upgrade_2017_03_06 extends AdminJobProcessor
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
        $conn->exec('DROP TABLE IF EXISTS `ar_postponed_report`');
        $conn->exec('
CREATE TABLE `ar_postponed_report`
(
	`ar_report_set_id` INTEGER  NOT NULL,
	`ar_organization_unit_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_report_set_id`,`ar_organization_unit_id`),
	CONSTRAINT `ar_postponed_report_FK_1`
		FOREIGN KEY (`ar_report_set_id`)
		REFERENCES `ar_report_set` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_postponed_report_FI_2` (`ar_organization_unit_id`),
	CONSTRAINT `ar_postponed_report_FK_2`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
		ON DELETE CASCADE
)Engine=tokudb COMPRESSION=tokudb_quicklz,DEFAULT CHARACTER SET = utf8, DEFAULT COLLATE = utf8_bin;
   ');

        $conn->exec('ALTER TABLE ar_report_scheduler ADD COLUMN minimum_cost BIGINT;');

        return '';

    }
}

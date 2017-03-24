<?php

class Upgrade_2017_03_10 extends AdminJobProcessor
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

        $conn->exec('ALTER TABLE ar_report_set ADD COLUMN postponed_reports INTEGER default 0 NOT NULL');
        $conn->exec('ALTER TABLE ar_report_set ADD COLUMN postponed_amount BIGINT default 0 NOT NULL');
        $conn->exec('ALTER TABLE ar_report_set ADD COLUMN reports INTEGER default 0 NOT NULL');
        $conn->exec('ALTER TABLE ar_report_set ADD COLUMN amount BIGINT default 0 NOT NULL');
        $conn->exec('ALTER TABLE ar_report_set ADD COLUMN postponed_fields_are_updated TINYINT default 1 NOT NULL');

        $conn->exec('DROP TABLE IF EXISTS `ar_postponed_report_tmp`;');
        $conn->exec('
CREATE TABLE `ar_postponed_report_tmp`
(
	`ar_organization_unit_id` INTEGER  NOT NULL,
	`from_date` DATETIME  NOT NULL,
	`is_billed` TINYINT  NOT NULL,
	`is_processed` TINYINT  NOT NULL,
	PRIMARY KEY (`ar_organization_unit_id`),
	CONSTRAINT `ar_postponed_report_tmp_FK_1`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
		ON DELETE CASCADE
)Engine=tokudb COMPRESSION=tokudb_quicklz,DEFAULT CHARACTER SET = utf8, DEFAULT COLLATE = utf8_bin;
');

        // Force a recalculation of all report-set derived fields
        $conn->exec('UPDATE ar_report_set SET postponed_fields_are_updated = 0;');

        $conn->exec('
CREATE TABLE `ar_expanded_extensions`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`ar_organization_unit_id` INTEGER,
	`extension_code` VARCHAR(5024)  NOT NULL,
	PRIMARY KEY (`id`),
	INDEX `ar_expanded_extensions_FI_1` (`ar_organization_unit_id`),
	CONSTRAINT `ar_expanded_extensions_FK_1`
		FOREIGN KEY (`ar_organization_unit_id`)
		REFERENCES `ar_organization_unit` (`id`)
)Engine=tokudb COMPRESSION=tokudb_quicklz,DEFAULT CHARACTER SET = utf8, DEFAULT COLLATE = utf8_bin;
        ');

        return '';
    }
}

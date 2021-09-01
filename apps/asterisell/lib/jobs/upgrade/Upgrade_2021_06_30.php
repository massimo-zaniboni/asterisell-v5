<?php

class Upgrade_2021_06_30 extends AdminJobProcessor
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
        
        
        $conn->exec('DROP TABLE IF EXISTS `ar_specific_rate_calc`;');
        $conn->exec('
CREATE TABLE `ar_specific_rate_calc`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`note` TEXT  NOT NULL,
        `ar_rate_id` INTEGER,
	`specific_rate_name` VARCHAR(255)  NOT NULL,
	`price_category_name` VARCHAR(255)  NOT NULL,
	`mediumtext_specific_rate_in_match_all` MEDIUMTEXT  NOT NULL,
	`mediumtext_specific_rate_in_match_exact` MEDIUMTEXT  NOT NULL,
	`mediumtext_specific_rate_out` MEDIUMTEXT  NOT NULL,
	`rate_plan_out` TEXT  NOT NULL,
	`mediumtext_base_rate_diff` MEDIUMTEXT  NOT NULL,
	`calc_info` TEXT  NOT NULL,
	`calc_error` TEXT,
	`is_recalc` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	INDEX `ar_specific_rate_calc_FI_1` (`ar_rate_id`),
	CONSTRAINT `ar_specific_rate_calc_FK_1`
		FOREIGN KEY (`ar_rate_id`)
		REFERENCES `ar_rate` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN `imported_info` VARCHAR(4096);');  
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN `exported_internal_telephone_number` VARCHAR(1024)  NOT NULL;');
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN `exported_billable_customer_ar_party_id` INTEGER;');
        $conn->exec('ALTER TABLE ar_cdr ADD COLUMN `from_source_cdr_id` INTEGER DEFAULT NULL;');

        $conn->exec('ALTER TABLE ar_source_cdr DROP PRIMARY KEY, ADD PRIMARY KEY (`calldate`,`id`);');
        $conn->exec('ALTER TABLE ar_source_cdr ADD COLUMN `is_hacked` tinyint NOT NULL DEFAULT 0;');
        $conn->exec('DROP TABLE IF EXISTS `ar_void`;');
        $conn->exec('CREATE TABLE `ar_void` (
                       `id` INTEGER  NOT NULL AUTO_INCREMENT,
                       PRIMARY KEY (`id`)
                     )ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;');
        
        
        ArTelephonePrefixPeer::createOrUpdatePrefix("Solidal", "Solidal", "39445", null);
        
        return '';
   }
}

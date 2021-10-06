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
        
        return '';
   }
}

<?php

class Upgrade_2019_07_24 extends AdminJobProcessor
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

        $conn->exec('
CREATE TABLE `ar_wholesale_carrier`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255),
	`note` VARCHAR(1024),
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_wholesale_carrier_U_1` (`internal_name`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        $conn->exec('
CREATE TABLE `ar_wholesale_number`
(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`telephone_number` VARCHAR(255)  NOT NULL,
	`from_date` DATETIME  NOT NULL,
	`exists` TINYINT default 1 NOT NULL,
	`extension_codes` VARCHAR(5024),
	`use_default_extension_codes` TINYINT,
	`ar_reseller_id` INTEGER,
	`ar_wholesale_carrier_id` INTEGER,
	`income_price` BIGINT default 0 NOT NULL,
	`cost_price` BIGINT default 0 NOT NULL,
	`csv_comment` VARCHAR(255),
	`csv_last_date` DATETIME,
	`csv_to_delete` TINYINT default 0 NOT NULL,
	`csv_is_current` TINYINT default 0 NOT NULL,
	PRIMARY KEY (`id`),
	KEY `ar_wholesale_number_I_1`(`from_date`),
	KEY `ar_wholesale_number_I_2`(`use_default_extension_codes`),
	KEY `ar_wholesale_number_I_3`(`csv_last_date`),
	KEY `p1_wholesale_number`(`telephone_number`, `from_date`),
	KEY `p2_wholesale_number`(`csv_last_date`, `ar_reseller_id`, `ar_wholesale_carrier_id`, `telephone_number`),
	KEY `p3_wholesale_number`(`from_date`, `ar_reseller_id`, `ar_wholesale_carrier_id`, `telephone_number`),
	INDEX `ar_wholesale_number_FI_1` (`ar_reseller_id`),
	CONSTRAINT `ar_wholesale_number_FK_1`
		FOREIGN KEY (`ar_reseller_id`)
		REFERENCES `ar_reseller` (`id`),
	INDEX `ar_wholesale_number_FI_2` (`ar_wholesale_carrier_id`),
	CONSTRAINT `ar_wholesale_number_FK_2`
		FOREIGN KEY (`ar_wholesale_carrier_id`)
		REFERENCES `ar_wholesale_carrier` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        $conn->exec('
CREATE TABLE `ar_wholesale_number_transaction`
(
	`from_date` DATETIME  NOT NULL,
	`count_numbers` INTEGER,
	`count_resellers` INTEGER,
	`count_carriers` INTEGER,
	`reseller_codes` VARCHAR(8048),
	PRIMARY KEY (`from_date`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        $conn->exec('
CREATE TABLE `ar_wholesale_number_imported_date_proc`
(
	`from_date` DATETIME  NOT NULL,
	PRIMARY KEY (`from_date`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        $conn->exec('
CREATE TABLE `ar_wholesale_number_transaction_to_update`
(
	`from_date` DATETIME  NOT NULL,
	PRIMARY KEY (`from_date`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        $conn->exec('
CREATE TABLE `ar_wholesale_update_proc`
(
	`foreign_id` INTEGER  NOT NULL,
	`csv_comment` VARCHAR(255),
	`csv_last_date` DATETIME,
	`csv_is_current` TINYINT  NOT NULL,
	PRIMARY KEY (`foreign_id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        $conn->exec('
CREATE TABLE `ar_wholesale_replace_proc`
(
	`from_date` DATETIME  NOT NULL,
	`ar_reseller_id` INTEGER,
	PRIMARY KEY (`from_date`),
	INDEX `ar_wholesale_replace_proc_FI_1` (`ar_reseller_id`),
	CONSTRAINT `ar_wholesale_replace_proc_FK_1`
		FOREIGN KEY (`ar_reseller_id`)
		REFERENCES `ar_reseller` (`id`)
)ENGINE=TokuDB ROW_FORMAT=TOKUDB_SNAPPY, DEFAULT CHARACTER SET = utf8mb4, DEFAULT COLLATE = utf8mb4_bin;
');

        return '';
    }
}

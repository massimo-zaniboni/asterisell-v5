<?php

class Upgrade_2017_01_24 extends AdminJobProcessor {

    public function isCDRTableModified()
    {
        return false;
    }

    public function isDBUpgradeJob() {
        return true;
    }

    public function process()
    {

        $conn = Propel::getConnection();

        $conn->exec('ALTER TABLE ar_report ADD COLUMN `ar_tag_id` INTEGER');
        $conn->exec('CREATE INDEX ar_report_FI_tag_id ON ar_report(`ar_tag_id`)');
        $conn->exec('ALTER TABLE ar_report ADD CONSTRAINT ar_report_FK_tag_id FOREIGN KEY (ar_tag_id) REFERENCES ar_tag(id) ');
	    $conn->exec('ALTER TABLE ar_params ADD COLUMN invoice_payment_due_in_xx_days INTEGER(4)');
	    $conn->exec('ALTER TABLE ar_party ADD COLUMN payment_iban VARCHAR(255)');
        $conn->exec('ALTER TABLE ar_party ADD COLUMN payment_bic VARCHAR(255)');
        $conn->exec('ALTER TABLE ar_party ADD COLUMN payment_info VARCHAR(255)');
        $conn->exec('ALTER TABLE ar_party ADD COLUMN payment_sepa VARCHAR(255)');

        $conn->exec('DROP TABLE IF EXISTS `ar_tag`');
        $conn->exec('CREATE TABLE `ar_tag`(
	`id` INTEGER  NOT NULL AUTO_INCREMENT,
	`internal_name` VARCHAR(255)  NOT NULL,
	`note_for_admin` VARCHAR(1024),
	`name_for_customer` VARCHAR(512) default \'\' NOT NULL,
	`note_for_customer` VARCHAR(1204) default \'\' NOT NULL,
	PRIMARY KEY (`id`),
	UNIQUE KEY `ar_tag_U_1` (`internal_name`)
)Engine=tokudb COMPRESSION=tokudb_quicklz,DEFAULT CHARACTER SET = utf8, DEFAULT COLLATE = utf8_bin;');

    $conn->exec('DROP TABLE IF EXISTS `ar_party_has_tag`;');
    $conn->exec('CREATE TABLE `ar_party_has_tag`(
	`ar_party_id` INTEGER  NOT NULL,
	`ar_tag_id` INTEGER  NOT NULL,
	PRIMARY KEY (`ar_party_id`,`ar_tag_id`),
	CONSTRAINT `ar_party_has_tag_FK_1`
		FOREIGN KEY (`ar_party_id`)
		REFERENCES `ar_party` (`id`)
		ON DELETE CASCADE,
	INDEX `ar_party_has_tag_FI_2` (`ar_tag_id`),
	CONSTRAINT `ar_party_has_tag_FK_2`
		FOREIGN KEY (`ar_tag_id`)
		REFERENCES `ar_tag` (`id`)
		ON DELETE CASCADE
)Engine=tokudb COMPRESSION=tokudb_quicklz,DEFAULT CHARACTER SET = utf8, DEFAULT COLLATE = utf8_bin;');

        return '';

    }
}

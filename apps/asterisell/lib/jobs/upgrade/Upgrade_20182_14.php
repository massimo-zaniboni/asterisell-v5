<?php

class Upgrade_2018_02_14 extends AdminJobProcessor
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

        $conn->exec('ALTER TABLE ar_party ADD COLUMN contract_number VARCHAR(255)');
        $conn->exec('ALTER TABLE ar_party ADD COLUMN legal_registration_number VARCHAR(255)');
        $conn->exec('ALTER TABLE ar_party ADD COLUMN contact_name VARCHAR(255)');
        $conn->exec('ALTER TABLE ar_party ADD COLUMN web_site VARCHAR(120)');
        $conn->exec('ALTER TABLE ar_rate_category CHANGE COLUMN internal_name internal_name VARCHAR(255)  NOT NULL');
        $conn->exec('ALTER TABLE ar_organization_unit ADD COLUMN internal_checksum1 VARCHAR(200)');
        $conn->exec('ALTER TABLE ar_organization_unit ADD COLUMN internal_checksum2 VARCHAR(200)');
        $conn->exec('ALTER TABLE ar_organization_unit ADD COLUMN internal_checksum3 VARCHAR(200)');
        $conn->exec('ALTER TABLE ar_user ADD COLUMN clear_password_to_import VARCHAR(200)');
        $conn->exec('CREATE INDEX ar_user_I_1 ON ar_user(clear_password_to_import)');
        $conn->exec('ALTER TABLE ar_cdr_provider ADD COLUMN last_imported_id INTEGER');
        $conn->exec('ALTER TABLE ar_cdr_provider ADD COLUMN last_imported_data VARCHAR(2048)');

        return '';
   }
}

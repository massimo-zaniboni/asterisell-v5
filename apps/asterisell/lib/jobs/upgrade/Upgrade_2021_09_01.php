<?php

class Upgrade_2021_09_01 extends AdminJobProcessor
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

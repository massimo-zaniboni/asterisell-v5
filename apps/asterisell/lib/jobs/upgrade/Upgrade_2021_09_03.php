<?php

class Upgrade_2021_09_03 extends AdminJobProcessor
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
        
        // NOTE: 0 is for fixed line number, and 3 for mobile
        // All other numbers can be special numbers.
        $this->createSpecialPrefix("391");
        $this->createSpecialPrefix("392");
        $this->createSpecialPrefix("394");
        $this->createSpecialPrefix("395");
        $this->createSpecialPrefix("396");
        $this->createSpecialPrefix("397");
        $this->createSpecialPrefix("398");
        $this->createSpecialPrefix("399");
        
        return '';
   }
   
   protected function createSpecialPrefix($p) {
        ArTelephonePrefixPeer::createOrUpdatePrefix("Special", "Italy", $p, '');
   }
   
}

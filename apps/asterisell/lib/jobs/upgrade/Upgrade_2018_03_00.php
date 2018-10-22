<?php

class Upgrade_2018_03_00 extends AdminJobProcessor
{

    // TODO replace this code with real upgrading code when it is all stable

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
        return '';
   }
}

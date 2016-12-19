<?php


/**
 * Do Nothing.
 */
class EmptyUpgradeJob extends AdminJobProcessor {

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        return '';
    }
}

<?php


/**
 * Force a fail in the upgrade process.
 * Usend mainly for testing.
 */
class FailUpgradeJob extends AdminJobProcessor {

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        $this->assertCondition(false, "Test a failure.");
        return '';
    }
}

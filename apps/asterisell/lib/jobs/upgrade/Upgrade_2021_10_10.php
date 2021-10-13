<?php

/**
 * Rename pending files to the new TWT format.
 */
class Upgrade_2021_10_10 extends AdminJobProcessor {

    public function isCDRTableModified() {
        return true;
    }

    public function isDBUpgradeJob() {
        return true;
    }

    public function process() {

        // match names like: t_1634051706-015495800-1144152.TWT-ftp__twt-cps__vAntelma
        $reg = '/^(t_\d+[-]\d+[-]\d+[.]TWT[-]ftp__twt[-])(cps|wlr|voip)__(vAntelma|v1)$/';

        $sourceFiles = scandir(ImportDataFiles::getAbsoluteInputDirectory());
        if ($sourceFiles === FALSE) {
            return '';
        }

        foreach ($sourceFiles as $sourceFile) {

            if ($sourceFile === '.' || $sourceFile === '..') {
                continue;
            }

            $m = array();
            if (preg_match($reg, $sourceFile, $m)) {
                $newVersion = 'v2';
                if (strcmp($m[3], "vAntelma") == 0) {
                    $newVersion = 'vAntelma2';
                }
                $newName = $m[1] . $m[2] . '__' . $newVersion;

                $file1 = ImportDataFiles::getAbsoluteInputDirectory() . $sourceFile;
                $file2 = ImportDataFiles::getAbsoluteInputDirectory() . $newName;

                rename($file1, $file2);
            }
        }

        return '';
    }
}

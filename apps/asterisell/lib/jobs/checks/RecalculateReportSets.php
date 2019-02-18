<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Recalculate report-sets fields.
 */
class RecalculateReportSets extends FixedJobProcessor
{

    public function process()
    {
        ArProblemException::garbageCollect(get_class($this), null, null);
        $log = $this->check();
        return $log;
    }

    private function check()
    {
        $prof = new JobProfiler('report-sets');

        $conn = Propel::getConnection();
        $upd = $conn->prepare('CALL proc_update_postponed_reportset_amounts(?)');

        $query = '
        SELECT id
        FROM ar_report_set
        WHERE postponed_fields_are_updated = 0;';

        $stm = $conn->prepare($query);
        $stm->execute();
        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
            $upd->execute(array($id));
            $upd->closeCursor();
            $prof->incrementProcessedUnits();
        }
        $stm->closeCursor();

        return $prof->stop();
    }
}

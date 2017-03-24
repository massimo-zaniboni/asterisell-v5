<?php

/* $LICENSE 2017:
 *
 * Copyright (C) 2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $id = $rs[0];
            $upd->execute(array($id));
            $upd->closeCursor();
            $prof->incrementProcessedUnits();
        }
        $stm->closeCursor();

        return $prof->stop();
    }
}

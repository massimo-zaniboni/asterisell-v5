<?php

/* $LICENSE 2013, 2014, 2015:
 *
 * Copyright (C) 2013, 2014, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Maintain only a limited set of saved BundleState in the past.
 *
 * The BundleState of already billed time frames are completely deleted, because they can be easily
 * recalculate in case of needs starting from empty bundle-state, and there is usually no need to store them.
 *
 * Delete other states, using a logaritmic approach.
 *
 * The rating engine start in any case from the nearest BundleState initial calldate.
 */
class GarbageCollectBundleState extends FixedJobProcessor
{
    const GARBAGE_KEY = 'GarbageCollectBundleState';

    public function process()
    {
        $prof = new JobProfiler('deleted old bundle states');

        $conn = Propel::getConnection();

        // Delete BundleStates before official call date.

        $d = $this->getOfficialCallDate();
        if (!is_null($d)) {
            $stm = $conn->prepare('DELETE FROM ar_bundle_state WHERE to_time < ?');
            $stm->execute(array(fromUnixTimestampToMySQLTimestamp($d)));
            $stm->closeCursor();
        }

        // Delete old BundleStates, using a logaritmic scale.

        $stm = $conn->prepare('SELECT id, to_time FROM ar_bundle_state ORDER BY to_time DESC LIMIT 5000');
        $stm->execute();

        /**
         * @var int[] $toDelete the id of bundle_state to delete
         */
        $toDelete = array();

        /**
         * @var int $intervalIndex a distance in seconds from now.
         * All the bundle-state until this value must be pruned, and only one bundle-state must be kept.
         */
        $intervalIndex = 2;

        $lastDate = null;

        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
            $id = $rs[0];
            $bundleDate = fromMySQLTimestampToUnixTimestamp($rs[1]);

            if (!is_null($lastDate)) {

                // how much old is the bundle-state
                $distance = $lastDate - $bundleDate;

                if ($distance > $intervalIndex) {
                    while ($distance > $intervalIndex) {
                        $intervalIndex *= 1.5;
                    }
                } else {
                    $toDelete[] = $id;
                }
            }

            $lastDate = $bundleDate;
        }
        $stm->closeCursor();

        // Delete the records.

        $stm = $conn->prepare('DELETE FROM ar_bundle_state WHERE id = ?');
        foreach ($toDelete as $id) {
            $prof->incrementProcessedUnits();
            $stm->execute(array($id));
        }

        return $prof->stop();
    }
}

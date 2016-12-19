<?php

/* $LICENSE 2009, 2010, 2011, 2012:
 *
 * Copyright (C) 2009, 2010, 2011, 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Read data from CSV Files containing CDRs data,
 * process them, and move them to a processed directory queue.
 *
 * This is an abstract/generic class used from other Job
 * to implement useful features.
 */
abstract class ImportCDRsFromCSVFileQueueProcessor extends FileMessageQueueProcessor
{

    /**
     * @return bool TRUE if duplicated lines in imported CSV files are recognized,
     *           and they are imported again according the new code, but without duplication.
     *           FALSE otherwise.
     */
    public function recognizeDuplicatedCSVLines()
    {
        return FALSE;
    }

    /**
     * @return string
     */
    protected function getDuplicatedCSVLinesMesage()
    {
        if ($this->recognizeDuplicatedCSVLines()) {
            return "NOTE: CSV lines already inserted in CDR table, will not be duplicated, but simply replaced with a new CDR version, according changes in the code. ";
        } else {
            return "If the file was partially processed, remove from it already processed parts, in order to avoid duplication of imported CDRs.";
        }
    }

    /**
     * Delete (if exists) a CDR with the same $sourceId.
     * @param string $sourceId
     * @return void
     */
    protected function deleteDuplicatedCDR($sourceId)
    {
        $connection = Propel::getConnection();

        $query = 'DELETE FROM ar_cdr WHERE source_id = ?';
        $stm = $connection->prepare($query);
        $stm->execute(array($sourceId));
    }

    /**
     * @param string $sourceId
     * @return bool
     */
    protected function thereIsDuplicateCDR($sourceId)
    {
        $connection = Propel::getConnection();
        $query = 'SELECT count(id) FROM ar_cdr WHERE source_id = ?';
        $stm = $connection->prepare($query);
        $stm->execute(array($sourceId));
        $count = intval($stm->fetchColumn());
        $stm->closeCursor();

        if ($count > 0) {
            return true;
        } else {
            return false;
        }
    }
}

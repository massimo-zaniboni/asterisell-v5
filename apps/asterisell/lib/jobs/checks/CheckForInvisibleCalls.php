<?php

/* $LICENSE 2009, 2013, 2014:
 *
 * Copyright (C) 2009, 2013, 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Check for calls without a join, and that are invisible to the system.
 *
 */
class CheckForInvisibleCalls extends FixedJobProcessor
{
    const GARBAGE_KEY = 'CheckForInvisibleCalls';

    public function process()
    {
        ArProblemException::garbageCollect(self::GARBAGE_KEY, null, null);

        $fieldsToTest = array(
            'ar_vendor_id',
            'ar_communication_channel_type_id',
            'ar_telephone_prefix_id'
        );

        $query = 'SELECT ar_cdr.id
        FROM ar_cdr
        WHERE destination_type <> ' . DestinationType::error
        . ' AND destination_type <> ' . DestinationType::ignored
        . ' AND (FALSE ';

        foreach ($fieldsToTest as $f) {
            $query .= ' OR ' . $f . ' IS NULL ';
        }

        $query .= ') LIMIT 1';
        $conn = Propel::getConnection();
        $stm = $conn->prepare($query);
        $stm->execute();

        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {

            $problemDuplicationKey =  self::GARBAGE_KEY . " - bad CDRs found";
            $problemDescription = 'There are calls that are signaled as correctly processed, but that have a bad format, with missing links to database fields. Used query for test is ' . $query;
            $problemEffect = 'These calls are not visible inside the call reports, and they are not signaled as errors, so they are lost. ';
            $problemProposedSolution = 'This is an error in the application code. Contact the assistance.';
            ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_CRITICAL, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, self::GARBAGE_KEY, null, null, $problemDescription, $problemEffect, $problemProposedSolution);
        }
        $stm->closeCursor();

        return '';
    }

}

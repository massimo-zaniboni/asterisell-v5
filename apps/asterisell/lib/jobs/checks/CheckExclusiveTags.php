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
 * Check if a customer has all the proper tags.
 */
abstract class CheckExclusiveTags extends FixedJobProcessor
{

    /**
     * @return array an array of pair array containing the two exclusive tags.
     * For example `array(array(10,11), array(20,21))` if a customer had to be tagged as 10 or 11 (if not it is an error,
     * if both it is an error), and as 20 or 21 (if not it is an error, if both it is an error).
     * NOTE: in this version of the code the supported TAGS are only pairs.
     */
    abstract public function getExclusiveTAGS();

    public function process()
    {
        $garbageKey = get_class($this);

        ArProblemException::garbageCollect($garbageKey, null, null);

         /**
         * @var array $xorTAGS a list of pairs (mandatory pairs) of exclusive TAGS
         */
        $xorTAGS = $this->getExclusiveTAGS();

        // Check if there are two exclusive tags set at the same time
        $sql1 = '
        SELECT p.id, p.name, t1.internal_name, t2.internal_name
        FROM ar_party as p
        , ar_tag as t1
        , ar_tag as t2
        , ar_party_has_tag as pt1
        , ar_party_has_tag as pt2
        WHERE
            t1.id = ?
        AND t2.id = ?
        AND pt1.ar_party_id = p.id
        AND pt2.ar_party_id = p.id
        AND pt1.ar_tag_id = t1.id
        AND pt2.ar_tag_id = t2.id';

        // Check if there is no tag set
        $sql2 = '
        SELECT p.id, p.name, t1.internal_name, t2.internal_name
        FROM ar_party as p
        , ar_tag as t1
        , ar_tag as t2
        WHERE
            t1.id = ?
        AND t2.id = ?
        AND p.id NOT IN (
          SELECT pt.ar_party_id
          FROM ar_party_has_tag AS pt
          WHERE pt.ar_tag_id = t1.id
          OR pt.ar_tag_id = t2.id
        )';

        // Find parties with problems.
        // This is a fast operation so it is done all the times.
        $conn = Propel::getConnection();
        foreach($xorTAGS as $tags) {
            list($tagId1, $tagId2) = $tags;

            $stm = $conn->prepare($sql1);
            $stm->execute(array($tagId1, $tagId2));
            while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
                $problemDuplicationKey = get_class($this) . " - " . $rs[0] . " - $tagId1 $tagId2 both ";
                $problemDescription = 'The party id ' . $rs[0] . ', with name "' . $rs[1] . '", has both tags ' . $rs[2] . ' and ' . $rs[3] . ' set at the same time.';
                $problemEffect = 'Reports and other jobs can not work properly, because they assume that a party has only one of the two tags set.';
                $problemProposedSolution = 'Set the correct tag for the specified party.';
                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR
                    , ArProblemDomain::VOIP_ACCOUNTS
                    , null
                    , $problemDuplicationKey
                    , $garbageKey
                    , null
                    , null
                    , $problemDescription
                    , $problemEffect
                    , $problemProposedSolution);
            }
            $stm->closeCursor();

            $stm = $conn->prepare($sql2);
            $stm->execute(array($tagId1, $tagId2));
            while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
                $problemDuplicationKey = get_class($this) . " - " . $rs[0] . " - $tagId1 $tagId2 none ";
                $problemDescription = 'The party id ' . $rs[0] . ', with name "' . $rs[1] . '", has no tag ' . $rs[2] . ' or ' . $rs[3] . '.';
                $problemEffect = 'Reports and other jobs can not work properly, because they assume that a party has one of the two tags set.';
                $problemProposedSolution = 'Set the correct tag for the specified party.';
                ArProblemException::createWithGarbageCollection(
                    ArProblemType::TYPE_ERROR
                    , ArProblemDomain::VOIP_ACCOUNTS
                    , null
                    , $problemDuplicationKey
                    , $garbageKey
                    , null
                    , null
                    , $problemDescription
                    , $problemEffect
                    , $problemProposedSolution);
            }
            $stm->closeCursor();
         }

        return '';
    }
}

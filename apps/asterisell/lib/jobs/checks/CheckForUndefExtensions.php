<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Advise the admin if there are undef extensions.
 */
class CheckForUndefExtensions extends FixedJobProcessor
{
    public function process()
    {
        $parentForUndef = ArOrganizationUnitPeer::retrieveByInternalName(ConfigureOrganizationHierarchy::UNSPECIFIED_EXTENSION_INTERNAL_NAME);

        if (!is_null($parentForUndef)) {

            $conn = Propel::getConnection();

            $query = 'SELECT DISTINCT(s.extension_name)
                  FROM ar_organization_unit AS o,
                       ar_organization_unit_has_structure AS s
                  WHERE s.ar_organization_unit_id = o.id
                  AND   s.ar_parent_organization_unit_id = ?
                  ';

            $stm = $conn->prepare($query);
            $stm->execute(array($parentForUndef->getId()));
            $count = 0;
            $codes = '';
            while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
                $count++;
                $codes .= $rs[0] . ', ';
            }
            $stm->closeCursor();

            if ($count > 0) {
                $problemDuplicationKey = get_class($this) . ' - ' . $count;
                $problemDescription = "There are $count extension codes (VoIP accountcode), that are not associated to any extension/organization, but that are used in CDRs to rate. They are created by the system, and put in the \"" . RateCalls::UNSPECIFIED_EXTENSION_HUMAN_READABLE_NAME . "\" system class. They are: $codes";
                $problemEffect = "The CDRs will be rated, because the extension are created by the system, but cost of the calls are not assigned to the correct extension and organization. ";
                $problemProposedSolution = "Define the extension codes, and rerate the CDRs. Note that some of these extensions can be instead external telephone numbers, that are not internal extensions. This because the CDRs merge and rating rules are not complete, and they classify some incoming calls in a bad way. In case, ignore these telephone numbers.";
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::VOIP_ACCOUNTS,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            }
        }
    }
}
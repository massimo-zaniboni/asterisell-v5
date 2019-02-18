<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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
            while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {
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
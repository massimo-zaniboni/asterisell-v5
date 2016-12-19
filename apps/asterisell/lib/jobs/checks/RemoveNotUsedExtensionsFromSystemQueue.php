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
 * Remove not used extensions from system queue.
 * In this way if there are improvements in recognition of extensions,
 * not used automatically imported extensions are removed from the system.
 */
class RemoveNotUsedExtensionsFromSystemQueue extends FixedJobProcessor
{
    public function process()
    {

        $profiler = new JobProfiler("extensions");
        $log = '';

        /**
         * @var PropelPDO $conn
         */
        $conn = Propel::getConnection();

        $timeFrameInMinutes = sfConfig::get('app_check_new_external_files_to_import_after_minutes');
        $checkFile = get_class($this);
        $checkLimit = strtotime("-$timeFrameInMinutes minutes");
        $mutex = new Mutex($checkFile);

        if ($mutex->maybeTouch($checkLimit)) {

            $system = ArOrganizationUnitPeer::retrieveByInternalName(ConfigureOrganizationHierarchy::UNSPECIFIED_EXTENSION_INTERNAL_NAME);

            if (!is_null($system)) {
                $systemId = $system->getId();

                $profiler->incrementProcessedUnits(1);
                $conn->beginTransaction();
                try {
                    ArProblemException::garbageCollect(get_class($this), null, null);

                    $stm = $conn->prepare('
                DELETE s, u
                FROM ar_organization_unit_has_structure AS s
                INNER JOIN ar_organization_unit AS u ON u.id = s.ar_organization_unit_id
                WHERE s.ar_parent_organization_unit_id = ?
                AND NOT EXISTS (SELECT ar_cdr.id
                                FROM ar_cdr
                                WHERE ar_cdr.ar_organization_unit_id = s.ar_organization_unit_id
                )');

                    $stm->execute(array($systemId));

                    $this->commitTransactionOrSignalProblem($conn);

                } catch (Exception $e) {
                    $this->maybeRollbackTransaction($conn);

                    $problemDuplicationKey = get_class($this);
                    $problemDescription = 'In class ' . get_class($this) . ' it was not delete not used extensions. The SQL error message ' . $e->getMessage() . "\n" . $e->getTraceAsString();
                    $problemProposedSolution = "If the error persist contact the assistance.";
                    $problemEffect = "Some extensions (internal telephone numbers), not used any more, are not removed from the system.";
                    $p = ArProblemException::createWithGarbageCollection(ArProblemType::TYPE_ERROR, ArProblemDomain::APPLICATION, null, $problemDuplicationKey, get_class($this), null, null, $problemDescription, $problemEffect, $problemProposedSolution);
                    throw($p);
                }
            }

            OrganizationUnitInfo::resetCachedValues();
        } else {
            $log .= "Process will be executed later, every $timeFrameInMinutes minutes, according application settings.";
        }

        return $log . $profiler->stop();
    }

}

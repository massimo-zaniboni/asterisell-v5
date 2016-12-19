<?php

require 'lib/model/om/BaseArRatingEngineExportStatusPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_rating_engine_export_status' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArRatingEngineExportStatusPeer extends BaseArRatingEngineExportStatusPeer {

    // DEV-NOTE: if you changes these constants, then MySQL procedures must be regenerated.

    const NUMBER_PORTABILITY_ID = 'number_portability';

    /**
     * @static
     * @param string $internalName
     * @param PropelPDO|null $pdo
     * @return ArRatingEngineExportStatus|null
     */
    public static function retrieveByInternalName($internalName, $pdo = null)
    {
        $criteria = new Criteria();
        $criteria->add(ArRatingEngineExportStatusPeer::INTERNAL_NAME, $internalName);

        return ArRatingEngineExportStatusPeer::doSelectOne($criteria, $pdo);
    }

} // ArRatingEngineExportStatusPeer

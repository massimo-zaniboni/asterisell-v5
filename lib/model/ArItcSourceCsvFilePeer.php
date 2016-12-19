<?php

require 'lib/model/om/BaseArItcSourceCsvFilePeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_itc_source_csv_file' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArItcSourceCsvFilePeer extends BaseArItcSourceCsvFilePeer {

    /**
     * Fast PDO access to the table, for returning the checksum value.
     *
     * @static
     * @param $name
     * @param null $conn
     * @return string|null
     */
    public static function getChecksumValue_fast($name, $conn = null) {
        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        /**
         * @var PDOStatement $stmt
         */
        $stmt = null;
        $q = 'SELECT checksum FROM ar_itc_source_csv_file WHERE name = ?';
        $stmt = $conn->prepare($q);
        $stmt->execute(array($name));

        $r = null;
        while ((($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false)) {
            $r = $rs[0];
        }
        $stmt->closeCursor();

        return $r;
    }

    /**
     * @static
     * @param string $internalName
     * @return ArSourceCsvFile|null
     */
    public static function retrieveByName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArItcSourceCsvFilePeer::NAME, $internalName);
        return ArItcSourceCsvFilePeer::doSelectOne($criteria);
    }


} // ArItcSourceCsvFilePeer

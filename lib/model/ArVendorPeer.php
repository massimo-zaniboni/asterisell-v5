<?php

require 'lib/model/om/BaseArVendorPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_vendor' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArVendorPeer extends BaseArVendorPeer {

    /**
     * A fast (cached) access to the name.
     * @param int $id
     * @return string the name of the communication channel
     */
    static public function getName($id) {
       static $cache = null;
       if (is_null($cache)) {
            $cache = array();
            $conn = Propel::getConnection();
            $query = 'SELECT v.id, p.name
                      FROM ar_vendor AS v
                      INNER JOIN ar_party AS p
                      ON v.ar_party_id = p.id';
            $stm = $conn->prepare($query);
            $stm->execute();
            while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
                $cache[$rs[0]] = $rs[1];
            }
           $stm->closeCursor();
       }

       if (array_key_exists($id, $cache)) {
           return $cache[$id];
       } else {
           return "unknwon";
       }
    }

    /**
     * @static
     * @param string $internalName
     * @return ArVendor|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArVendorPeer::INTERNAL_NAME, $internalName);

        return ArVendorPeer::doSelectOne($criteria);
    }

} // ArVendorPeer

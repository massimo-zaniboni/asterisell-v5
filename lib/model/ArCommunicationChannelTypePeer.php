<?php

require 'lib/model/om/BaseArCommunicationChannelTypePeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_communication_channel_type' table.
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArCommunicationChannelTypePeer extends BaseArCommunicationChannelTypePeer {

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
            $query = 'SELECT id, name FROM ar_communication_channel_type';
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
     * A fast (cached) access to the name.
     * @param int $id
     * @return string the name of the communication channel
     */
    static public function getInternalName($id) {
       static $cache = null;
       if (is_null($cache)) {
            $cache = array();
            $conn = Propel::getConnection();
            $query = 'SELECT id, internal_name FROM ar_communication_channel_type';
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
     * @param string $name
     * @return ArCommunicationChannelType|null
     */
    public static function retrieveByName($name)
    {
        static $cache = array();

        if (! array_key_exists($name, $cache)) {
            $criteria = new Criteria();
            $criteria->add(ArCommunicationChannelTypePeer::NAME, $name);
            $cache[$name] = ArCommunicationChannelTypePeer::doSelectOne($criteria);
        }

        return $cache[$name];
    }


    /**
     * @static
     * @param string $internalName
     * @return ArCommunicationChannelType|null
     */
    public static function retrieveByInternalName($internalName)
    {
        static $cache = array();

        if (! array_key_exists($internalName, $cache)) {
            $criteria = new Criteria();
            $criteria->add(ArCommunicationChannelTypePeer::INTERNAL_NAME, $internalName);

            $cache[$internalName] = ArCommunicationChannelTypePeer::doSelectOne($criteria);

        }

        return $cache[$internalName];
    }


} // ArCommunicationChannelTypePeer

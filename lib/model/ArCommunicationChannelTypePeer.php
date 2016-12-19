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

<?php

require 'lib/model/om/BaseArCommunicationChannelType.php';


/**
 * Skeleton subclass for representing a row from the 'ar_communication_channel_type' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArCommunicationChannelType extends BaseArCommunicationChannelType {

    public function __toString()
    {
        return $this->getName();
    }

    /**
     * @static
     * @param string $internalName
     * @param PDO|null $conn
     * @return ArCommunicationChannelType|null
     */
    public static function retrieveByInternalName($internalName, $conn = null)
    {
        $criteria = new Criteria();
        $criteria->add(ArCommunicationChannelTypePeer::INTERNAL_NAME, $internalName);

        return ArCommunicationChannelTypePeer::doSelectOne($criteria, $conn);
    }

} // ArCommunicationChannelType

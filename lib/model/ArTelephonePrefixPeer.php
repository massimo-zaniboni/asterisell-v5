<?php

/**
 * Subclass for performing query and update operations on the 'ar_telephone_prefix' table.
 *
 *
 *
 * @package lib.model
 */
class ArTelephonePrefixPeer extends BaseArTelephonePrefixPeer
{

    /**
     * Retrieve a single object by pkey.
     *
     * @param      int $pk the exact prefix.
     * @param      PropelPDO $con the connection to use
     * @return     ArTelephonePrefix
     */
    public static function retrieveByPrefix($pk, PropelPDO $con = null)
    {

        if ($con === null) {
            $con = Propel::getConnection(ArTelephonePrefixPeer::DATABASE_NAME, Propel::CONNECTION_READ);
        }

        $criteria = new Criteria(ArTelephonePrefixPeer::DATABASE_NAME);
        $criteria->add(ArTelephonePrefixPeer::PREFIX, $pk);

        $v = ArTelephonePrefixPeer::doSelect($criteria, $con);

        return !empty($v) > 0 ? $v[0] : null;
    }

    /**
     * Remove the prefix and derived prefixes.
     * @param string $prefix the prefixes to remove.
     */
    public static function deleteAllPrefixes($prefix)
    {
        $conn = Propel::getConnection();

        $stmt = $conn->prepare('DELETE FROM ar_telephone_prefix WHERE prefix LIKE ?');
        $stmt->execute(array($prefix . '%'));
    }

    /**
     * Cache and return an array filter.
     * DEV-NOTE: this code requires that the ar_cached_organization_info is invalidated in case
     * of changes in telephone prefix table.
     * @return array
     */
    public static function getCachedGeographicLocationFilterArray()
    {
        return self::getCachedFilterArray("geographic_location");
    }

    /**
     * Cache and return an array filter.
     * DEV-NOTE: this code requires that the ar_cached_organization_info is invalidated in case
     * of changes in telephone prefix table.
     * @return array
     */
    public static function getCachedOperatorTypeFilterArray()
    {
        return self::getCachedFilterArray("operator_type");
    }

    /**
     * Cache and return an array filter.
     * DEV-NOTE: this code requires that the ar_cached_organization_info is invalidated in case
     * of changes in telephone prefix table.
     * @param string $fieldName
     * @return array
     */
    protected static function getCachedFilterArray($fieldName)
    {
        static $cache = array();

        // DEV-NOTE: this code must be unique for this type of data.
        $code = 'tp_' . $fieldName;
        $r = null;

        if (array_key_exists($code, $cache)) {
            $r = $cache[$code];
        } else {

            $conn = Propel::getConnection();
            $query = "SELECT content FROM ar_cached_organization_info WHERE internal_name = ?";
            $stm = $conn->prepare($query);
            $stm->execute(array($code));
            while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
                try {
                    $r = unserialize($rs[0]);

                    if ($r === FALSE) {
                        $r = null;
                    } else if (!is_array($r)) {
                        $r = null;
                    }

                } catch (Exception $e) {
                    $r = null;
                }
            }
            $stm->closeCursor();

            // Calculate the value, and store in the cache

            if (is_null($r)) {
                $r = array("" => "");

                $query = "SELECT DISTINCT $fieldName FROM ar_telephone_prefix ORDER by $fieldName";
                $stm = $conn->prepare($query);
                $stm->execute(array($code));
                while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
                    $r[$rs[0]] = __(trim($rs[0]));
                }
                $stm->closeCursor();

                $query = "INSERT INTO ar_cached_organization_info(internal_name, content) VALUES(?, ?);";
                $stm = $conn->prepare($query);
                $stm->execute(array($code, serialize($r)));
                $stm->closeCursor();

            }

            $cache[$code] = $r;
        }

        return $r;
    }


}

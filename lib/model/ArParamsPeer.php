<?php

/**
 * Subclass for performing query and update operations on the 'ar_params' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArParamsPeer extends BaseArParamsPeer
{
    /**
     * @static
     * @return ArParams|null
     */
    static public function getDefaultParams() {
        $c = new Criteria();
        $c->add(ArParamsPeer::IS_DEFAULT, TRUE);
        return ArParamsPeer::doSelectOne($c);
    }

    /**
     * @return int|null
     */
    static public function getDefaultParamsId() {
      static $defaultId = null;

        if (is_null($defaultId)) {
            $conn = Propel::getConnection();
            $query = 'SELECT id FROM ar_params WHERE is_default';
            $stmt = $conn->prepare($query);
            $stmt->execute();
            $results = $stmt->fetchAll(PDO::FETCH_COLUMN);
            foreach($results as $result) {
                $defaultId = $result[0];
            }
        }

        return $defaultId;
    }
}

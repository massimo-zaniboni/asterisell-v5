<?php

/**
 * Subclass for performing query and update operations on the 'ar_party' table.
 *
 *
 *
 * @package lib.model
 */
class ArPartyPeer extends BaseArPartyPeer
{

    /**
     * @return array int => party name ordered by party name, to use in a selector
     */
    static public function getPartiesSelector()
    {
        static $r = null;

        if (is_null($r)) {

            $conn = Propel::getConnection();
            $query = "SELECT id, name FROM ar_party ORDER BY name";
            $stm = $conn->prepare($query);
            $stm->execute();
            $r = array('' => '');

            while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
                $r[$rs[0]] = $rs[1];
            }

            $stm->closeCursor();
        }

        return $r;
    }

}

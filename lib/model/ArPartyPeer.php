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

    /**
     * @param int $partyId
     * @param int $tagId
     * @return bool true if the party has the specified TAG
     */
    static public function hasTag($partyId, $tagId)
    {

        $conn = Propel::getConnection();
        $query = 'SELECT ar_party_id FROM ar_party_has_tag WHERE ar_party_id = ? AND ar_tag_id = ?';
        $stm = $conn->prepare($query);
        $stm->execute(array($partyId, $tagId));
        $r = false;
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $r = true;
        }
        $stm->closeCursor();
        return $r;
    }

    /**
     * @param int $partyId
     * @return string|null party.contract_number
     */
    static public function getPartyContract($partyId)
    {
        $conn = Propel::getConnection();
        $query = "SELECT contract_number FROM ar_party WHERE id = ?";
        $stm = $conn->prepare($query);
        $stm->execute(array($partyId));
        $r = null;
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $r = $rs[0];
        }

        $stm->closeCursor();

        return $r;
    }

}

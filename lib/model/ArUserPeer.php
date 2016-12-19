<?php

class ArUserPeer extends BaseArUserPeer
{

    static public function getRootUser() {
        $c = new Criteria();
        $c->add(ArUserPeer::IS_ROOT_ADMIN, true);
        return ArUserPeer::doSelectOne($c);
    }

    /**
     * @param string $login
     * @return ArUser|null
     */
    static public function retrieveByLogin($login) {
        $c = new Criteria();
        $c->add(ArUserPeer::LOGIN, $login);
        return ArUserPeer::doSelectOne($c);
    }
}

<?php

require 'lib/model/om/BaseArProblemResponsible.php';


class ArProblemResponsible extends BaseArProblemResponsible {

    const ADMIN = 10;

    const APPLICATION_ASSISTANCE = 20;

    /**
     * Used mainly during database initialization for loading values.
     *
     * @static
     * @return string[] names associated to types
     */
    static public function getTypeNames() {
        static $arr = null;

        if (is_null($arr)) {
            $arr = array(
                self::ADMIN => "Administrator"
            ,  self::APPLICATION_ASSISTANCE=> "Application Assistance"
            );
        }

        return $arr;
    }

    /**
     * @param int $typeId
     * @return string
     */
    static public function getTypeName($typeId) {
        $arr = self::getTypeNames();
        return $arr[$typeId];
    }

    public function __toString() {
        return self::getTypeName($this->getId());
    }

} // ArProblemResponsible

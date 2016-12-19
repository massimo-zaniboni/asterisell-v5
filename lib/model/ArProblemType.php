<?php

class ArProblemType extends BaseArProblemType {

    // NOTE: if you change these fields, update also the C and Haskell rating code.

    const TYPE_INFO = 10;

    const TYPE_WARNING = 20;

    const TYPE_ERROR = 30;

    const TYPE_CRITICAL = 40;

    const TYPE_INTERNAL_LOG = 50;

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
               self::TYPE_INFO => "Info"
            ,  self::TYPE_WARNING => "Warning"
            ,  self::TYPE_ERROR => "Error"
            ,  self::TYPE_CRITICAL => "Critical"
            ,  self::TYPE_INTERNAL_LOG => 'Internal Log'
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

} // ArProblemType

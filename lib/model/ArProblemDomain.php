<?php

require 'lib/model/om/BaseArProblemDomain.php';


class ArProblemDomain extends BaseArProblemDomain
{

    // IMPORTANT: if you change this constant, update also the corresponding C rating code.

    const VOIP_ACCOUNTS = 10;

    const RATES = 20;

    const CALL_FLOW_MERGING_RULES = 30;

    const APPLICATION = 40;

    const REPORTS = 50;

    const CONFIGURATIONS = 60;

    const SAFETY = 70;

    /**
     * Used mainly during database initialization for loading values.
     *
     * @static
     * @return string[] names associated to types
     */
    static public function getTypeNames()
    {
        static $arr = null;

        if (is_null($arr)) {
            $arr = array(
                self::VOIP_ACCOUNTS => "VoIP Accounts"
            , self::RATES => "Rates"
            , self::CALL_FLOW_MERGING_RULES => "Call Flow Merging Rules"
            , self::APPLICATION => "Application Code"
            , self::REPORTS => "Reports"
            , self::CONFIGURATIONS => "Configurations"
            , self::SAFETY => "Safety"
            );
        }

        return $arr;
    }

    /**
     * @param int $typeId
     * @return string
     */
    static public function getTypeName($typeId)
    {
        $arr = self::getTypeNames();
        return $arr[$typeId];
    }

    public function __toString()
    {
        return self::getTypeName($this->getId());
    }

} // ArProblemDomain

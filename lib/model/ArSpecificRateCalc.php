<?php

require 'lib/model/om/BaseArSpecificRateCalc.php';


/**
 * Skeleton subclass for representing a row from the 'ar_specific_rate_calc' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArSpecificRateCalc extends BaseArSpecificRateCalc {

    public function getMediumtextSpecificRateOutContent()
    {
        return $this->getMediumtextSpecificRateOut();  
    }

    public function getMediumtextBaseRateDiffContent()
    {
        return $this->getMediumtextBaseRateDiff();
    }
    
} // ArSpecificRateCalc

<?php

require 'lib/model/om/BaseArSpecificRateCalcPeer.php';


/**
 * Skeleton subclass for performing query and update operations on the 'ar_specific_rate_calc' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArSpecificRateCalcPeer extends BaseArSpecificRateCalcPeer {

    /**
     * @param string $baseRateName the rate to use as base. Use the last/current date.
     * @param string $specificRateName the rate to use as specific rate. Use the last/current date.
     * The content of the rate is assumed to match all prefixes, i.e. if there is 
     * a "39" then it is assumed as "39*".
     * @param string $rateName the resultinng rate name
     * @return the specific ArRate to save or null
     */
    static public function updateSpecificRateCalcFromRateNames($baseRateName, $specificRateName, $rateName) {
        $specificRate = ArRatePeer::retrieveByInternalName($specificRateName);
        $rateCalcId = self::createNewSpecificRateCalcFromRateNames($baseRateName, $specificRateName, $rateName);
        
        if (self::calcSpecificRate($rateCalcId)) {
           $rateCalc = ArSpecificRateCalcPeer::retrieveByPK($rateCalcId);
           $specificRate->setSourceDataFileContentFromPlainText($rateCalc->getMediumtextSpecificRateOutContent());
           $specificRate->setHTMLDescriptionInPlainText('');
           return $specificRate;
        } else {
          return null;
        }
    }
    
    /**
     * @param string $baseRateName the rate to use as base. Use the last/current date.
     * @param string $specificRateName the rate to use as specific rate. Use the last/current date.
     * The content of the rate is assumed to match all prefixes, i.e. if there is 
     * a "39" then it is assumed as "39*".
     * @param string $rateName the resultinng rate name
     * @return int the id of the specific rate to use for calculating the compressed specific rate 
     */
    static public function createNewSpecificRateCalcFromRateNames($baseRateName, $specificRateName, $rateName) {
        $baseRate = ArRatePeer::retrieveByInternalName($baseRateName);
        $specificRate = ArRatePeer::retrieveByInternalName($specificRateName);
        
        return self::createNewSpecificRateCalcFromRateIds($baseRate->getId(), $specificRate->getId(), $rateName);
    }
    
    /**
     * @param int $baseRateId the rate to use as base
     * @param int $specificRateId the rate to use as specific rate.
     * The content of the rate is assumed to match all prefixes, i.e. if there is 
     * a "39" then it is assumed as "39*".
     * @param string $rateName the resulting rate name
     * @return int the id of the specific rate to use for calculating the compressed specific rate 
     */
    static public function createNewSpecificRateCalcFromRateIds($baseRateId, $specificRateId, $rateName) {
        $specificRate = ArRatePeer::retrieveByPK($specificRateId);
        $baseRate = ArRatePeer::retrieveByPK($baseRateId);
        
        $specificRateName = $specificRate->getInternalName();
        $baseRateName = $baseRate->getInternalName();
        
        if (isEmptyOrNull($specificRateName)) {
            $specificRateName = "id-" . $specificRate->getId();
        }
        
        if (isEmptyOrNull($baseRateName)) {
            $baseRateName = "id-" . $baseRate->getId();
        }
        
        $rateCalc = new ArSpecificRateCalc();
        $rateCalc->setMediumtextSpecificRateInMatchExact('description,operator,prefix,cost_by_minute,cost_on_call');
        $rateCalc->setMediumtextBaseRateDiff('');     // there is no default value, so create them
        $rateCalc->setMediumtextSpecificRateOut('');
        $rateCalc->setCalcError('');
        $rateCalc->setCalcInfo('');
        $rateCalc->setRatePlanOut('');
        $rateCalc->setSpecificRateName($rateName);
        $rateCalc->setPriceCategoryName($rateName);
        $rateCalc->setNote("Compress $specificRateName with base rate $baseRateName");        
        $rateCalc->setArRateId($baseRateId);
        $rateCalc->setMediumtextSpecificRateInMatchAll($specificRate->getSourceDataFileContentInPlainText());
        $rateCalc->save();

        return $rateCalc->getId();
    }

    /**
     * @param int $rateCalcId the rate to complete
     * @return bool true if it is all ok
     */
    static public function calcSpecificRate($rateCalcId) {
        // Reset calculated fields
        $rateCalc = ArSpecificRateCalcPeer::retrieveByPk($rateCalcId);
        $rateCalc->setMediumtextBaseRateDiff('');
        $rateCalc->setMediumtextSpecificRateOut('');
        $rateCalc->setCalcError('');
        $rateCalc->setCalcInfo('');
        $rateCalc->setRatePlanOut('');
        $rateCalc->save();
        
        // Call Haskell code
        $ratingParams = array();
        $currencyPrecision = sfConfig::get('app_currency_decimal_places');
        
        $cmd = RateEngineService::getToolExecutable() 
                . ' --calc-specific-rate ' . $rateCalcId 
                . ' --params ' . RateEngineService::writeParams($ratingParams)
                . ' --currency-precision ' . $currencyPrecision
                ;
        
        $output = array();
        $exitStatus = 0;
        exec($cmd, $output, $exitStatus);

        ArSpecificRateCalcPeer::clearInstancePool();
        
        if ($exitStatus != 0) {
          $errorInfo = implode("\n", $output);

          $rateCalc = ArSpecificRateCalcPeer::retrieveByPk($rateCalcId);
          $rateCalc->setCalcError($errorInfo);
          $rateCalc->save();
          
          return false;
        }
        
        return true;
    }
    
} // ArSpecificRateCalcPeer

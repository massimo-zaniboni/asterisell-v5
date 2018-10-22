<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Init/update rates of a certain type/category/vendor.
 */
abstract class InitRates extends AdminJobProcessor
{

    /**
     * @return string the internal name used for classifying all rates of this type.
     */
    abstract public function getIdForRateType();

    /**
     * @abstract
     * @return int when the new rates became active and replace the old rates of the same type.
     */
    abstract public function getStartTime();

    /**
     * Add the new rates.
     *
     * post condition: the added rates must have as internal_name the value `getIdForRateType()`
     * post condition: the added rates must have as starting date the `getStartTime()`
     * post condition: the added rates must have null as ending date
     * .
     * @abstract
     * @return string log message
     */
    abstract protected function addNewRates();

    ///////////////////////
    // UTILITY FUNCTIONS //
    ///////////////////////

    protected $disableReratingFlag = false;

    public function disableRerating()
    {
        $this->disableReratingFlag = true;
    }

    /**
     * @return ArRate to complete and save
     * @throw ArProblemException
     */
    protected function createFreeRate()
    {

        $date = $this->getStartTime();

        $rm = new PhpRateByDuration();
        $rm->rateByMinute = false;
        $rm->costForMinute = 0;
        $rm->costOnCall = 0;
        $rm->atLeastXSeconds = 0;
        $rm->whenRound_0_59 = 0;
        $rm->externalTelephonePrefix = "";

        $r = new ArRate();
        $r->setInternalName($this->getIdForRateType());
        $r->setStartTime(fromUnixTimestampToMySQLTimestamp($date));
        $r->setEndTime(null);
        $r->setIsException(false);
        $r->serializePhpRateMethod($rm);

        return $r;
    }

    ////////////////////////
    // INTERNAL FUNCTIONS //
    ////////////////////////

    /**
     * Disable the rates, setting a specific end date.
     *
     * @param string $internalName
     * @param int $date when disable the rate
     */
    protected function disableRate($internalName, $date)
    {

        $c = new Criteria();
        $c->add(ArRatePeer::INTERNAL_NAME, $internalName);
        $c->add(ArRatePeer::END_TIME, null);
        $rs = ArRatePeer::doSelect($c);

        foreach ($rs as $rate) {
            /**
             * @var ArRate $rate
             */
            $rate->setEndTime(fromUnixTimestampToMySQLTimestamp($date));
            $rate->save();
        }
    }

    /////////////////////////
    // ADMIN JOB INTERFACE //
    /////////////////////////

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        $this->disableRate($this->getIdForRateType(), $this->getStartTime());

        $log = $this->addNewRates();

        if (!$this->disableReratingFlag) {
            // generate a rerate event, in order to use the new rates
            self::rerateCalls($this->getStartTime());
            self::setWaitForScheduledRerate(false);
        }

        return get_class($this) . ': ' . $log;
    }
}

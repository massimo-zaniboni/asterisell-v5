<?php

require 'lib/model/om/BaseArInstanceStatus.php';

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Skeleton subclass for representing a row from the 'ar_instance_status' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArInstanceStatus extends BaseArInstanceStatus {

    /**
     * @retun int 100 for all calls rated
     */
    public function getPercCorrectOutgoingPreviousMonth() {
      return round(($this->getPropertyNrOutgoingPreviousMonth() / ($this->getPropertyNrErrorsOutgoingPreviousMonth() + $this->getPropertyNrOutgoingPreviousMonth())) * 100);
    }

    /**
     * @retun int 100 for all calls rated
     */
    public function getPercCorrectIncomingPreviousMonth() {
        return round(($this->getPropertyNrIncomingPreviousMonth() / ($this->getPropertyNrErrorsIncomingPreviousMonth() + $this->getPropertyNrIncomingPreviousMonth())) * 100);
    }

    /**
     * @retun int 100 for all calls rated
     */
    public function getPercCorrectInternalPreviousMonth() {
        return round(($this->getPropertyNrInternalPreviousMonth() / ($this->getPropertyNrErrorsInternalPreviousMonth() + $this->getPropertyNrInternalPreviousMonth())) * 100);
    }

    /**
     * @retun int 100 for all calls rated
     */
    public function getPercCorrectOutgoingLast30Days() {
        return round(($this->getPropertyNrOutgoingLast30Days() / ($this->getPropertyNrErrorsOutgoingLast30Days() + $this->getPropertyNrOutgoingLast30Days())) * 100);
    }

    /**
     * @retun int 100 for all calls rated
     */
    public function getPercCorrectIncomingLast30Days() {
        return round(($this->getPropertyNrIncomingLast30Days() / ($this->getPropertyNrErrorsIncomingLast30Days() + $this->getPropertyNrIncomingLast30Days())) * 100);
    }

    /**
     * @retun int 100 for all calls rated
     */
    public function getPercCorrectInternalLast30Days() {
        return round(($this->getPropertyNrInternalLast30Days() / ($this->getPropertyNrErrorsInternalLast30Days() + $this->getPropertyNrInternalLast30Days())) * 100);
    }

    /**
     * @retun string 100 for all calls rated
     */
    public function getStrPercCorrectOutgoingPreviousMonth() {
        $p = $this->getPercCorrectOutgoingPreviousMonth();
        return $p . '%';
    }

    /**
     * @retun string '100%' for all calls rated
     */
    public function getStrPercCorrectIncomingPreviousMonth() {
        $p = $this->getPercCorrectIncomingPreviousMonth();
        return $p . '%';
    }

    /**
     * @retun string '100%' for all calls rated
     */
    public function getStrPercCorrectInternalPreviousMonth() {
        $p = $this->getPercCorrectInternalPreviousMonth();
        return $p . '%';
    }

    /**
     * @retun string '100%' for all calls rated
     */
    public function getStrPercCorrectOutgoingLast30Days() {
        $p = $this->getPercCorrectOutgoingLast30Days();
        return $p . '%';
    }

    /**
     * @retun string '100%' for all calls rated
     */
    public function getStrPercCorrectIncomingLast30Days() {
        $p = $this->getPercCorrectIncomingLast30Days();
        return $p . '%';
    }

    /**
     * @retun string '100%' for all calls rated
     */
    public function getStrPercCorrectInternalLast30Days() {
        $p = $this->getPercCorrectInternalLast30Days();
        return $p . '%';
    }

    /**
     * @return int
     */
    public function getHoursOfDifferenceBetweenNowAndLastInfoUpdate() {
        $now = time();
        $lastInfo = fromMySQLTimestampToUnixTimestamp($this->getInfoTimestamp());

        $diff = $now - $lastInfo;

        return intval($diff / (60 * 60));
    }

    /**
     * @return int
     */
    public function getHoursOfDifferenceBetweenLastInfoUdpateAndLastRatedCdr() {
        $now = fromMySQLTimestampToUnixTimestamp($this->getInfoTimestamp());

        $lastInfo = fromMySQLTimestampToUnixTimestamp($this->getLastProcessedCdrTimestamp());

        $diff = $now - $lastInfo;

        return intval($diff / (60 * 60));
    }
    

} // ArInstanceStatus

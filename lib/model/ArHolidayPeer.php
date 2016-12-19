<?php

require 'lib/model/om/BaseArHolidayPeer.php';


class ArHolidayPeer extends BaseArHolidayPeer {

    /**
     * @param int $date
     * @return bool true if it is an holiday date
     */
    static public function isHoliday($date) {

        /**
         * @var array int as keys with 1 as Monday and 7 as Sunday
         */
        static $cachedWeekdays = null;

        /**
         * @var array Month-Day in ISO format as keys
         */
        static $cachedDaysAndMonth = null;

        /**
         * @var array date in ISO format as keys
         */
        static $cachedDates = null;

        if (is_null($cachedWeekdays)) {
            // init the caches

            $cachedWeekdays = array();
            $cachedDates = array();
            $cachedDaysAndMonth = array();

            $holidays = ArHolidayPeer::doSelect(new Criteria());
            foreach($holidays as $h) {
                /**
                 * @var ArHoliday $h
                 */

                $year = intval($h->getYear());
                $dayOfMonth = intval($h->getDayOfMonth());
                $month = intval($h->getMonth());
                $dayOfWeek = intval($h->getDayOfWeek());

                if ($dayOfWeek == 0) {
                    if ($year == 0) {
                        $cachedDaysAndMonth[$month . '-' . $dayOfMonth] = true;
                    } else {
                        $cachedDates[$year . '-' . $month . '-' . $dayOfMonth] = true;
                    }
                } else {
                    $cachedWeekdays[$dayOfWeek] = true;
                }

            }
        }

        $dateDayOfMonth = intval(date('d', $date));
        $dateMonth = intval(date('n', $date));
        $dateYear = intval(date('Y', $date));

        $dateDayOfWeek = intval(date('w', $date));
        if ($dateDayOfWeek == 0) {
            $dateDayOfWeek = 7;
        }

        if (isset($cachedWeekdays[$dateDayOfWeek])) {
            return true;
        }

        if (isset($cachedDaysAndMonth[$dateMonth . '-' . $dateDayOfMonth])) {
            return true;
        }

        if (isset($cachedDates[$dateYear . '-' . $dateMonth . '-' . $dateDayOfMonth])) {
            return true;
        }

        return false;
    }

} // ArHolidayPeer

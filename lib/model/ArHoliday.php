<?php

require 'lib/model/om/BaseArHoliday.php';


class ArHoliday extends BaseArHoliday
{

    /**
     * @return string
     */
    public function getHumanReadableDescription()
    {

        $year = intval($this->getYear());
        $dayOfMonth = intval($this->getDayOfMonth());
        $month = intval($this->getMonth());
        $dayOfWeek = intval($this->getDayOfWeek());

        if ($dayOfWeek == 0) {
            if ($year == 0) {
                $ref = strtotime('2000-' . $month . '-' . $dayOfMonth);
                return $dayOfMonth . date('S', $ref) . ' ' . date('F', $ref) . ', every year';
            } else {
                $ref = strtotime($year . '-' . $month . '-' . $dayOfMonth);
                return date('c', $ref);
            }
        } else {
            if ($dayOfWeek == 7) {
                $n = 'Sunday';
            } else if ($dayOfWeek == 1) {
                $n = 'Monday';
            } else if ($dayOfWeek == 2) {
                $n = 'Tuesday';
            } else if ($dayOfWeek == 3) {
                $n = 'Wednesday';
            } else if ($dayOfWeek == 4) {
                $n = 'Thursday';
            } else if ($dayOfWeek == 5) {
                $n = 'Friday';
            } else if ($dayOfWeek == 6) {
                $n = 'Saturday';
            } else {
                return 'unknown day';
            }

            return 'every ' . $n;
        }
    }


} // ArHoliday

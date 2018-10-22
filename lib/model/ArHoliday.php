<?php

require 'lib/model/om/BaseArHoliday.php';


class ArHoliday extends BaseArHoliday
{

    const OFF_PEAK = "off-peak";

    /**
     * @return string
     */
    public function getHumanReadableDescription()
    {

        $n = 'Match ';

        $year = intval($this->getYear());
        $dayOfMonth = intval($this->getDayOfMonth());
        $month = intval($this->getMonth());
        $dayOfWeek = intval($this->getDayOfWeek());

        if ($year == 0) {
            $n .= "every year, ";
        } else {
            $n .= "year $year, ";
        }

        if ($month == 0) {
            $n .= "every month, ";
        } else {
            $n .= "month $month, ";
        }

        if ($dayOfMonth == 0) {
            $n .= "every day of month, ";
        } else {
            $n .= "day $dayOfMonth,";
        }

        if ($dayOfWeek == 7) {
            $dn = 'Sunday';
        } else if ($dayOfWeek == 1) {
            $dn = 'Monday';
        } else if ($dayOfWeek == 2) {
            $dn = 'Tuesday';
        } else if ($dayOfWeek == 3) {
            $dn = 'Wednesday';
        } else if ($dayOfWeek == 4) {
            $dn = 'Thursday';
        } else if ($dayOfWeek == 5) {
            $dn = 'Friday';
        } else if ($dayOfWeek == 6) {
            $dn = 'Saturday';
        } else {
            $dn = 'unknown day';
        }

        if ($dayOfWeek == 0) {
            $n .= "";
        } else {
            $n .= "on " . $dn . ", ";
        }

        $fromH = $this->getFromHour();
        $fromM = $this->getFromMinutes();
        $toH = $this->getToHour();
        $toM = $this->getToMinutes();

        if (is_null($fromH)) {
            $fromH = 0;
        }
        if (is_null($fromM)) {
            $fromM = 0;
        }
        if (is_null($toH)) {
            $toH = 23;
        }
        if (is_null($toM)) {
            $toM = 59;
        }
        $t1 = strtotime("2000-01-01 " . $fromH . ":" . $fromM . ":00");
        $t2 = strtotime("2000-01-01 " . $toH . ":" . $toM . ":00");

        $n .= "from " . date('H:i', $t1) . " to " . date("H:i", $t2);

        return $n;
    }


} // ArHoliday

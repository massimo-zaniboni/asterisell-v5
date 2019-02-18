<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


/**
 * Define default communication channels.
 */
class ConfigureHolidays extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        // The first of year
        $h = new ArHoliday();
        $h->setDayOfMonth(1);
        $h->setMonth(1);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Christmash
        $h = new ArHoliday();
        $h->setDayOfMonth(25);
        $h->setMonth(12);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Christmash + 1
        $h = new ArHoliday();
        $h->setDayOfMonth(26);
        $h->setMonth(12);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Every Sunday
        $h = new ArHoliday();
        $h->setDayOfWeek(7);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Every Saturday afternoon
        $h = new ArHoliday();
        $h->setDayOfWeek(6);
        $h->setFromHour(14);
        $h->setFromMinutes(0);
        $h->setToHour(23);
        $h->setToMinutes(59);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Every Saturday morning, before working hours
        $h = new ArHoliday();
        $h->setDayOfWeek(6);
        $h->setFromHour(0);
        $h->setFromMinutes(0);
        $h->setToHour(8);
        $h->setToMinutes(0);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Every morning, before working hours
        $h = new ArHoliday();
        $h->setFromHour(0);
        $h->setFromMinutes(0);
        $h->setToHour(8);
        $h->setToMinutes(0);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        // Every night, after working hours
        $h = new ArHoliday();
        $h->setFromHour(19);
        $h->setFromMinutes(0);
        $h->setToHour(23);
        $h->setToMinutes(59);
        $h->setPeakCode(ArHoliday::OFF_PEAK);
        $h->save();

        return '';
    }

}
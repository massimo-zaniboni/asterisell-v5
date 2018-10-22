<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


/**
 * Define default communication channels.
 */
class ConfigurePortugalHolidays extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        $h = new ArHoliday();
        $h->setDayOfMonth(25);
        $h->setMonth(4);
        $h->save();

        $h = new ArHoliday();
        $h->setDayOfMonth(1);
        $h->setMonth(6);
        $h->save();

        $h = new ArHoliday();
        $h->setDayOfMonth(10);
        $h->setMonth(6);
        $h->save();

        $h = new ArHoliday();
        $h->setDayOfMonth(1);
        $h->setMonth(7);
        $h->save();

        $h = new ArHoliday();
        $h->setDayOfMonth(5);
        $h->setMonth(10);
        $h->save();

        $h = new ArHoliday();
        $h->setDayOfMonth(1);
        $h->setMonth(12);
        $h->save();

        $h = new ArHoliday();
        $h->setDayOfMonth(8);
        $h->setMonth(12);
        $h->save();

        return '';
    }

}
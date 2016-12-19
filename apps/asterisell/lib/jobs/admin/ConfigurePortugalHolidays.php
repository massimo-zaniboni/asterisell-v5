<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

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
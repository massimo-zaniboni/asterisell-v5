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

class InitTelephonePrefixes extends AdminJobProcessor
{

    const SPECIAL_TELEPHONE_NUMBERS_NAME = 'Service';

    /**
     * Used as prefix of internal extensions.
     */
    const INTERNAL_EXTENSION_PREFIX = 'extension-';

    /**
     * Used as prefix for special (emergency like), telephone numbers.
     */
    const SPECIAL_TELEPHONE_NUMBER_PREFIX = 'special-';

    const ANONYMOUS_NUMBER_PREFIX = ArTelephonePrefix::ANONYMOUS_TELEPHONE_NUMBER;

    const EMERGENCY_NUMBER_PREFIX = 'emergency-';

    const SERVICE_NUMBER_PREFIX = 'service-';

    const FREE_CALL_PREFIX = 'free-call-';

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {

        $this->createPrefix('SIP', mytr('Local Network'), ArTelephonePrefix::SYSTEM_LOCAL_PREFIX);
        $this->createPrefix(mytr('Unknown'), mytr('Unknown'), ArTelephonePrefix::ANONYMOUS_TELEPHONE_NUMBER);


        $this->createPrefix(self::SPECIAL_TELEPHONE_NUMBERS_NAME, 'Emergency', self::EMERGENCY_NUMBER_PREFIX);
        $this->createPrefix(self::SPECIAL_TELEPHONE_NUMBERS_NAME, 'Service', self::SERVICE_NUMBER_PREFIX);
        $this->createPrefix(self::SPECIAL_TELEPHONE_NUMBERS_NAME, 'Free Call', self::FREE_CALL_PREFIX);

        return '';
    }

    /**
     * @param string $pType
     * @param string $pPlace
     * @param string $pPrefix
     * @param string|null $pOperator
     * @return void
     */
    protected function createPrefix($pType, $pPlace, $pPrefix, $pOperator = null)
    {
        $r = new ArTelephonePrefix();
        $r->setPrefix($pPrefix);
        $r->setName($pOperator);
        $r->setGeographicLocation($pPlace);
        $r->setOperatorType($pType);
        $r->save();
    }
}

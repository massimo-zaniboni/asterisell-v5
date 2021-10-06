<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

        ArTelephonePrefixPeer::createOrUpdatePrefix("Solidal", "Italy", "39445", null);

        // NOTE: 0 is for fixed line number, and 3 for mobile
        // All other numbers can be special numbers.
        $this->createSpecialPrefix("391");
        $this->createSpecialPrefix("392");
        $this->createSpecialPrefix("394");
        $this->createSpecialPrefix("395");
        $this->createSpecialPrefix("396");
        $this->createSpecialPrefix("397");
        $this->createSpecialPrefix("398");
        $this->createSpecialPrefix("399");
    
        return '';
    }
    
    protected function createSpecialPrefix($p) {
        ArTelephonePrefixPeer::createOrUpdatePrefix("Special", "Italy", $p, '');
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
        ArTelephonePrefixPeer::createOrUpdatePrefix($pType, $pPlace, $pPrefix, $pOperator);
    }

}

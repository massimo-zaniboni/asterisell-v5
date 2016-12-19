<?php

/**
 * Subclass for representing a row from the 'ar_telephone_prefix' table.
 *
 *
 *
 * @package lib.model
 */
class ArTelephonePrefix extends BaseArTelephonePrefix
{
    /**
     * An Internal Prefix used for identifying local telephone numbers.
     */
    const SYSTEM_LOCAL_PREFIX = 'local-';

    /**
     * Telephone number used for anonymous incoming calls.
     */
    const ANONYMOUS_TELEPHONE_NUMBER = 'anonymous-';

    /**
     * @param string $operatorType
     * @param string $geographicLocation
     * @param string $name
     * @return string descriptive name of the operator, something like "Mobile - Italy - Vodafone".
     * NOTE: this function is used in order to avoid
     * in certain part of the program the deserialization
     * of many ArTelephonePrefix objects.
     */
    static public function calcDescriptiveName($operatorType, $geographicLocation, $name)
    {

        $sep = '';

        $r = '';

        if (!is_null($geographicLocation) && strlen(trim($geographicLocation)) != 0) {
            $r .= $geographicLocation;
            $sep = ' - ';
        }

        if (!is_null($operatorType) && strlen(trim($operatorType)) != 0) {
            $r .= $sep . $operatorType;
            $sep = ' - ';
        }

        if (!is_null($name) && strlen(trim($name)) != 0) {
            $r .= $sep . $name;
            $sep = ' - ';
        }

        return $r;
    }

    /**
     * @return string descriptive name of the operator, something like "Mobile - Italy - Vodafone"
     */
    public function getDescriptiveName()
    {
        return ArTelephonePrefix::calcDescriptiveName($this->getOperatorType(),
            $this->getGeographicLocation(),
            $this->getName());
    }
}

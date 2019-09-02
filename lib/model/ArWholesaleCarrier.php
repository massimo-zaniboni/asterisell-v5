<?php

require 'lib/model/om/BaseArWholesaleCarrier.php';


/**
 * Skeleton subclass for representing a row from the 'ar_wholesale_carrier' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArWholesaleCarrier extends BaseArWholesaleCarrier {


    public function __toString()
    {
        return ($this->getInternalName());
    }

} // ArWholesaleCarrier

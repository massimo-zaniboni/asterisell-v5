<?php

require 'lib/model/om/BaseArWholesaleNumber.php';


/**
 * Skeleton subclass for representing a row from the 'ar_wholesale_number' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArWholesaleNumber extends BaseArWholesaleNumber {

    const DELETE_SPECIAL_CODE = 'DELETE';
    const FREE_SPECIAL_CODE = '';

    const ALL_EFFECT = 'ALL';
    const RESELLER_EFFECT = 'RESELLER';
    const COMMENT_EFFECT = 'COMMENT';

	/**
	 * Initializes internal state of ArWholesaleNumber object.
	 * @see        parent::__construct()
	 */
	public function __construct()
	{
		// Make sure that parent constructor is always invoked, since that
		// is where any default values for this object are set.
		parent::__construct();
	}

    public function __toString()
    {
        return ($this->getTelephoneNumber());
    }

} // ArWholesaleNumber

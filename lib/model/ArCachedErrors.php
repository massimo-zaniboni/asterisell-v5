<?php

require 'lib/model/om/BaseArCachedErrors.php';


/**
 * Skeleton subclass for representing a row from the 'ar_cached_errors' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArCachedErrors extends BaseArCachedErrors {

	/**
	 * Initializes internal state of ArCachedErrors object.
	 * @see        parent::__construct()
	 */
	public function __construct()
	{
		// Make sure that parent constructor is always invoked, since that
		// is where any default values for this object are set.
		parent::__construct();
	}

} // ArCachedErrors

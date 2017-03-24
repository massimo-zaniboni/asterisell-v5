<?php

require 'lib/model/om/BaseArReportSet.php';


/**
 * Skeleton subclass for representing a row from the 'ar_report_set' table.
 *
 *
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArReportSet extends BaseArReportSet
{

    const SHOW_ONLY_TO_REVIEW_PARAM_NAME = 'to-review';

    /**
     * Initializes internal state of ArReportSet object.
     * @see        parent::__construct()
     */
    public function __construct()
    {
        // Make sure that parent constructor is always invoked, since that
        // is where any default values for this object are set.
        parent::__construct();
    }

    public function delete(PropelPDO $conn = null)
	{
        // overwrite standard method, because if there are associated reports
        // a trigger error is reported.

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        $this->deleteAssociatedReports($conn);
        parent::delete($conn);
    }

    public function deleteAssociatedReports(PDO $conn)
    {
        if (! is_null($this->getId())) {
            ArReportSetPeer::deleteAssociatedReports($this->getId(), $conn);
        }
    }

    /**
     * Set the flag, and publish the reports in case.
     *
     * @param bool $v
     * @return ArReportSet|void
     */
    public function setMustBeReviewed($v) {
       if ($this->getMustBeReviewed() !== $v) {
           ArReportSetPeer::publishReportToUsers($this->getId(), $v, false, Propel::getConnection());
       }
       parent::setMustBeReviewed($v);
    }


} // ArReportSet

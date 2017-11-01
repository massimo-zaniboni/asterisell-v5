<?php

require 'lib/model/om/BaseArVendor.php';


/**
 * Skeleton subclass for representing a row from the 'ar_vendor' table.
 *
 *
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArVendor extends BaseArVendor
{

    /**
     * Used in `internal_name` field.
     */
    const ID_FOR_DEFUAULT = 'default';

    /**
     * Initializes internal state of ArVendor object.
     * @see        parent::__construct()
     */
    public function __construct()
    {
        // Make sure that parent constructor is always invoked, since that
        // is where any default values for this object are set.
        parent::__construct();
    }

    protected $cachedName = null;

    public function getName()
    {
        if (is_null($this->cachedName)) {

            if (!is_null($this->getArParty())) {
                $name = $this->getArParty()->getFullName();
            } else {
                $name = $this->getInternalName();
                if (isEmptyOrNull($name)) {
                    $name .= 'vendor-' . $this->getId();
                }
            }

            $this->cachedName = $name;
        }

        return $this->cachedName;
    }

    /**
     * @return string
     */
    public function  getNameAsHtmlLink()
    {
        return '<a href="' . url_for('vendor/edit?id=' . $this->getId()) . '">' . htmlspecialchars($this->getName(), ENT_QUOTES, 'UTF-8') . '</a>';
    }

    public function __toString()
    {
        return $this->getName();
    }

} // ArVendor

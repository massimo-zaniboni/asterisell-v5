<?php

/**
 * wholesale_numbers actions.
 *
 * @package    asterisell
 * @subpackage wholesale_numbers
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class wholesale_numbersActions extends autoWholesale_numbersActions
{
     /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArWholesaleNumberPeer::ID);
    }

    protected function updateArWholesaleNumberFromRequest() {
        parent::updateArWholesaleNumberFromRequest();

        $value1 = $this->getRequestParameter('my_cost_editor');
        if (is_null($value1)) {
            $value1 = 0;
        }
        $value2 = from_local_decimal_to_php_decimal($value1);
        $value3 = from_php_decimal_to_db_decimal($value2);
        $this->ar_wholesale_number->setCostPrice($value3);

        $value1 = $this->getRequestParameter('my_income_editor');
        if (is_null($value1)) {
            $value1 = 0;
        }
        $value2 = from_local_decimal_to_php_decimal($value1);
        $value3 = from_php_decimal_to_db_decimal($value2);
        $this->ar_wholesale_number->setIncomePrice($value3);

        $id = $this->getRequestParameter('select_reseller');
        $this->ar_wholesale_number->setArResellerId($id);

        $id = $this->getRequestParameter('select_wholesale_carrier');
        $this->ar_wholesale_number->setArWholesaleCarrierId($id);

        if (isEmptyOrNull($this->ar_wholesale_number->getExtensionCodes())) {
            $this->ar_wholesale_number->setExtensionCodes(null);
            $useDefaultExtensionCodes = null;
            // use NULL for signaling that the field must be completed
        } else {
            $useDefaultExtensionCodes = ! $this->ar_wholesale_number->isColumnModified(ArWholesaleNumberPeer::EXTENSION_CODES);
            // NOTE: the codes where explicitely modified, so they are not anymore the default values
        }
        $this->ar_wholesale_number->setUseDefaultExtensionCodes($useDefaultExtensionCodes);
    }
}

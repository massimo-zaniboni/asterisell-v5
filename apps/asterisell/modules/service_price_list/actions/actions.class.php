<?php

/**
 * service_price_list actions.
 *
 * @package    asterisell
 * @subpackage service_price_list
 * @author     Your name here
 * @version    SVN: $Id: actions.class.php 12474 2008-10-31 10:41:27Z fabien $
 */
class service_price_listActions extends autoService_price_listActions
{


    /**
     * @param Criteria $c
     */
    protected function addSortCriteria($c)
    {
        // force a sort on ID for viewing all the calls in the LIMIT pagination

        parent::addSortCriteria($c);
        $c->addAscendingOrderByColumn(ArServicePricePeer::ID);
    }

    protected function updateArServicePriceFromRequest() {
        $value1 = $this->getRequestParameter('my_price_editor');
        if (is_null($value1)) {
            $value1 = 0;
        }

        $value2 = from_local_decimal_to_php_decimal($value1);
        $value3 = from_php_decimal_to_db_decimal($value2);
        $this->ar_service_price->setPrice($value3);

        $serviceId = $this->getRequestParameter('select_service');
        $this->ar_service_price->setArServiceId($serviceId);

        parent::updateArServicePriceFromRequest();
    }

}

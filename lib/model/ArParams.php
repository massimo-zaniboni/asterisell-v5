<?php

sfLoader::loadHelpers(array('Asterisell'));

/**
 * Subclass for representing a row from the 'ar_params' table.
 *
 *
 *
 * @package lib.model
 */
class ArParams extends BaseArParams
{
    public function __toString()
    {
        return $this->getName();
    }

    public function getVatTaxPercAsPhpDecimal()
    {
        return from_db_decimal_to_smart_php_decimal($this->getVatTaxPerc());
    }

    public function setVatTaxPercAsPhpDecimal($d)
    {
        $this->setVatTaxPerc(from_php_decimal_to_db_decimal($d));
    }

    /**
     * @return     string|null
     */
    public function getLogoImageInInvoicesWithCompletePath()
    {
        $f = trim($this->logo_image_in_invoices);

        if (is_null($f)) {
            return null;
        } else if (strlen($f) == 0) {
            return null;
        } else {
            if (substr($f, 0, 1) == '/') {
                // Remove all the text until uploads string

                $pos = strpos($f, '/uploads/');
                if ($pos !== FALSE) {
                    $f = substr($f, $pos);
                }
                return sfConfig::get('sf_web_dir') . $f;
            } else {
                return sfConfig::get('sf_web_dir') . '/images/' . $f;
            }
        }
    }
}

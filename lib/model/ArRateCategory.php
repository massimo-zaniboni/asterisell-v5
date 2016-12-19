<?php

/**
 * Subclass for representing a row from the 'ar_rate_category' table.
 *
 * 
 *
 * @package lib.model
 */ 
class ArRateCategory extends BaseArRateCategory
{

    /**
     * Used in `internal_name` field.
     */
  const ID_FOR_NORMAL = 'normal';

    /**
     * Used in `internal_name` field.
     */
  const ID_FOR_DISCOUNTED = 'discounted';

  public function __toString() {
    return $this->getInternalName();
  }

    /**
     * @return string
     */
    public function getName() {
      return $this->getInternalName();
  }
}

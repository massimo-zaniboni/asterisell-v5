<?php

// SPDX-License-Identifier: GPL-3.0-or-later

/**
 * Two values are valid only if there is exactly one value with a no-null value.
 *
 * Validator params:
 *     check:  field
 *     msg:    error message
 */
class ExactlyOneNotNullValidator extends sfValidator {
  /**
   * Executes this validator.
   *
   * // XXX It does not return false...
   *
   * @param mixed A file or parameter value/array
   * @param compare_error An error message reference
   *
   * @return bool true, if this validator executes successfully, otherwise false
   */
  public function execute(&$value, &$error) {
    $check_param = $this->getParameterHolder()->get('check');
    $check_value = $this->getContext()->getRequest()->getParameter($check_param);
    $t = 0;
    if ((!is_null($value)) || (strlen(trim($value)) > 0)) {
      $t++;
    }
    if ((!is_null($check_value)) || (strlen(trim($check_value)) > 0)) {
      $t++;
    }
    if ($t == 1) {
      return true;
    } else {
      $error = $this->getParameterHolder()->get('msg');
      return false;
    }
  }
  public function initialize($context, $parameters = null) {
    // initialize parent
    parent::initialize($context);
    // set defaults
    $this->getParameterHolder()->set('compare_error', 'Invalid input');
    $this->getParameterHolder()->add($parameters);
    return true;
  }
}
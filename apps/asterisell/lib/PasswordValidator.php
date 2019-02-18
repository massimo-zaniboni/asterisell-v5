<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('Asterisell'));

class PasswordValidator extends sfValidator
{
  public function execute (&$value, &$error)
  {
    if (areAllValidCharacters($value)) {
        return TRUE;
    } else {
       $error = $this->getParameter('password_error');
       return FALSE;
    }
  }
 
  public function initialize ($context, $parameters = null)
  {
    // Initialize parent
    parent::initialize($context);
 
    $this->setParameter('password_error', 'Field contains illegal characters.');
 
    // Set parameters
    $this->getParameterHolder()->add($parameters);
 
    return true;
  }
}
?>
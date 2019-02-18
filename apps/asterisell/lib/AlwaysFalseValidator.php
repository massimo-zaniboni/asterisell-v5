<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * Two values are valid only if there is exactly one value with a no-null value.
 *
 * Validator params:
 *     check:  field
 *     msg:    error message
 */
class AlwaysFalseValidator extends sfValidator
{
    /**
     * Executes this validator.
     *
     * @param mixed $value a file or parameter value/array
     * @param mixed $error compare_error An error message reference
     *
     * @return bool true, if this validator executes successfully, otherwise false
     */
    public function execute(&$value, &$error)
    {
        $error = "errore";
        return false;
    }

    public function initialize($context, $parameters = null)
    {
        // initialize parent
        parent::initialize($context);
        return true;
    }
}
<?php

/* $LICENSE 2012:
 *
 * Copyright (C) 2012 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */


/**
 * Encapusaleted a string, for an easy call with `get(ESC_RAW)`.
 */
class EncapsulatedForRawOutput
{
    /**
     * @var string
     */
    protected $v = '';

    /**
     * @param string $v
     */
    public function set($v) {
        $this->v = $v;
    }

    /**
     * @return string
     */
    public function get() {
        return $this->v;
    }
}
?>
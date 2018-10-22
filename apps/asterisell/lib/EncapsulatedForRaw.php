<?php

// SPDX-License-Identifier: GPL-3.0-or-later


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
<?php

/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class ChangeOrganizationInfoEvent extends JobData
{

    const INTERNAL_NAME_IN_JOB_QUEUE = "ChangeOrganizationInfo";

    /**
     * @var null|string
     */
    protected $yamlContent = null;

    /**
     * @param string|null $c
     */
    public function setYAMLContent($c) {
        $this->yamlContent = $c;
    }

    /**
     * @return string|null
     */
    public function getYAMLContent() {
        return $this->yamlContent;
    }

    public function getDescription()
    {
        return self::INTERNAL_NAME_IN_JOB_QUEUE;
    }
}

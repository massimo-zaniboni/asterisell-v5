<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

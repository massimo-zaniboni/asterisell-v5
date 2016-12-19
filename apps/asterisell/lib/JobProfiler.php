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

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Used for profiling Job execution time.
 */
class JobProfiler
{

    /**
     * @var float
     */
    protected $startTime;

    /**
     * @var int
     */
    protected $countProcessedUnits;

    /**
     * @var string
     */
    protected $jobUnitName;

    public $exactTime;

    /**
     * Start the profiler.
     *
     * @param string $jobUnitName the name for the single unit of job. Something like "rates", "CDRs" and so on.
     */
    public function __construct($jobUnitName) {
        $this->jobUnitName = $jobUnitName;
        $this->startTime = microtime_float();
        $this->countProcessedUnits = 0;
    }

    public function incrementProcessedUnits()
    {
        $this->countProcessedUnits++;
    }

    /**
     * @param int $v
     * @return void
     */
    public function addToProcessedUnits($v) {
        $this->countProcessedUnits += $v;
    }

    /**
     * Stop the internal job profiler.
     *
     * @return string a description of the processing speed.
     */
    public function stop()
    {
        $jobUnitName = $this->jobUnitName;
        $nr = $this->countProcessedUnits;

        $time1 = $this->startTime;
        $time2 = microtime_float();

        $totTime = $time2 - $time1;
        $this->exactTime = $totTime;

        $totSeconds = intval(floor($totTime));
        $totTimeS = from_seconds_to_nice_duration($totSeconds);

        $jobsForSecondS = $nr;
        if ($nr > 0 && $totSeconds > 0) {
                $jobsForSecond = $nr / $totSeconds;
                $jobsForSecondS = intval(floor($jobsForSecond));
        }

        $usedMegaBytes = ceil(memory_get_peak_usage(false) / (1024 * 1024));

        return "$nr $jobUnitName processed in $totTimeS, $jobsForSecondS $jobUnitName processed for second, using $usedMegaBytes MB of RAM.";
    }

    public function getProcessedUnits() {
        return $this->countProcessedUnits;
    }
}

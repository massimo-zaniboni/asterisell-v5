<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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

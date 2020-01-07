<?php

/**
 * Subclass for representing a row from the 'ar_job_queue' table.
 *
 * @package lib.model
 */
class ArJobQueue extends BaseArJobQueue
{

    const TODO = 0;
    const RUNNING = 1;
    const DONE = 2;
    const ERROR = 3;

    /**
     * @return string
     */
    public function getTypeDescription()
    {
        $t = $this->getState();
        if ($t == ArJobQueue::TODO) {
            return "TODO";
        } else if ($t == ArJobQueue::RUNNING) {
            return "RUNNING";
        } else if ($t == ArJobQueue::DONE) {
            return "DONE";
        } else if ($t == ArJobQueue::ERROR) {
            return "ERROR";
        } else {
            return "!!!";
        }
    }

    /**
     * Remove the job from the queue.
     *
     * @param int $state
     * @param string|null $description
     * @return void
     */
    public function complete($state, $description = null) {
        $this->startNewState($state, $description);
    }

    /**
     * Remove the job from the queue.
     *
     * @param int $state
     * @param string|null $description
     * @return void
     */
    public function startNewState($state, $description = null) {
        $this->setState($state);
        if (!is_null($description)) {
          $this->setDescription($description);
        }
        $this->setEndAt(date("c"));
        $this->save(ArProblemException::getLogConnection());
    }

    public function setDescription($v)
    {
        $maxLen = 800;
        if ((!is_null($v)) && strlen($v) > $maxLen) {
            $v2 = substr($v, 0, $maxLen) . " [... truncated ...]";
        } else {
            $v2 = $v;
        }

        return parent::setDescription($v2);
    }

    /**
     * @return JobData|null an unserialized JobData, null if it does not exists or it is in a bad format.
     */
    public function unserializeDataJob()
    {
        $ds = $this->getPhpDataJobSerialization();
        if (!is_null($ds)) {
            $d = unserialize($ds);
            if ($d === false) {
                return null;
            } else {
                return $d;
            }
        } else {
            return null;
        }
    }
}

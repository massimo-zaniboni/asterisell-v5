<?php

/**
 * Subclass for performing query and update operations on the 'ar_job_queue' table.
 *
 *
 *
 * @package lib.model
 */
class ArJobQueuePeer extends BaseArJobQueuePeer
{
    /**
     * Add a new job on the queue.
     * @param JobData $d the data describing the job.
     * @param int|null $parentId the ArJobQueue.id of the main job, NULL if this job is already a main (top-level) job.
     * @param int $state
     * @param string $description
     * @param string|null $internalName
     * @return ArJobQueue the created job
     */
    static public function addNewWithStateAndDescription(JobData $d, $parentId, $state, $description, $internalName = null)
    {
        $job = new ArJobQueue();
        $job->setState($state);
        $job->setCreatedAt(date("c"));
        $job->setDescription($description);
        $job->setPhpDataJobSerialization(serialize($d));
        $job->setIsPartOf($parentId);
        $job->setInternalName($internalName);
        $job->save(ArProblemException::getLogConnection());

        if (is_null($parentId)) {
            $newParentId = $job->getId();
            $job->setIsPartOf($newParentId);
            $job->save(ArProblemException::getLogConnection());
        }

        return $job;
    }

    /**
     * Add a new job on the queue.
     *
     * @param JobData $d the data describing the job.
     * @param int|null $parentId the ArJobQueue.id of the main job, NULL if this job is already a main (top-level) job.
     * @param string|null $internalName
     * @return ArJobQueue the created job
     */
    static public function addNew(JobData $d, $parentId, $internalName = null)
    {
        return self::addNewWithStateAndDescription($d, $parentId, ArJobQueue::TODO, $d->getDescription(), $internalName);
    }

    /**
     * @static
     * @param string $code
     * @return ArJobQueue|null
     */
    public static function retrieveByInternalName($code)
    {
        $criteria = new Criteria();
        $criteria->add(ArJobQueuePeer::INTERNAL_NAME, $code);
        $criteria->add(ArJobQueuePeer::STATE, ArJobQueue::TODO);

        $job = ArJobQueuePeer::doSelectOne($criteria, ArProblemException::getLogConnection());

        if (!is_null($job)) {
            $job->startNewState(ArJobQueue::RUNNING);
        }

        return $job;
    }

    /**
     * @return ArJobQueue[]
     */
    public static function getRunningJobs()
    {
        $c = new Criteria();
        $c->add(ArJobQueuePeer::STATE, ArJobQueue::RUNNING);
        return ArJobQueuePeer::doSelect($c, ArProblemException::getLogConnection());
    }

    /**
     * @static
     * @return ArJobQueue|null
     */
    public static function getFirstJobInTheQueue()
    {
        static $c = null;

        if (is_null($c)) {
            // Note: the ascending order on is_part_of allows ordering jobs according their dependencies.
            $c = new Criteria();
            $c->addAscendingOrderByColumn(ArJobQueuePeer::IS_PART_OF);
            $c->add(ArJobQueuePeer::STATE, ArJobQueue::TODO);
        }
        $job = ArJobQueuePeer::doSelectOne($c, ArProblemException::getLogConnection());

        if (!is_null($job)) {
            $job->startNewState(ArJobQueue::RUNNING);
        }

        return $job;
    }

    /**
     * @return int|null null if there are no events
     */
    public static function getLastEventId()
    {
        $conn = ArProblemException::getLogConnection();
        $stm = $conn->prepare('SELECT MAX(id) FROM ar_job_queue WHERE state=?');
        $stm->execute(array(ArJobQueue::TODO));

        $r = null;
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $r = $rs[0];
            if (! is_null($r)) {
                $r = intval($r);
            }
        }
        $stm->closeCursor();

        return $r;
    }
}

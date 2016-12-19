<?php

/* $LICENSE 2009, 2010, 2011:
 *
 * Copyright (C) 2009, 2010, 2011 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Implement a MUTEX using the ar_lock table and PID process.
 *
 * NOTE: previous code was using `flock` PHP instruction, but it does not work, because
 * in Unix its behaviour is not mandatory.
 *
 * NOTE: this behaviour is not ATOMIC, but it is used for avoid the contemporary starting of multiple cron-job process,
 * or multiple user process. In any case also if the user start a job contemporary to the cron-job process,
 * there are no catastrophic events.
 */
class Mutex
{

    /**
     * @var bool
     */
    protected $removeLock = FALSE;

    /**
     * @var null|string
     */
    protected $name = NULL;

    /**
     * Init a mutex with a given name.
     *
     * @param string $name the name of the lock
     */
    public function __construct($name)
    {
        $this->name = $name;
    }

    /**
     * Release always the locked resources on mutex destruction.
     */
    public function __destruct()
    {
        $this->unlock();
    }

    /**
     * Lock. Only a single process can acquire and release a lock.
     * Dead process are recognized, and lock is disabled.
     *
     * @require call $this->unlock() when resources are no more needed.
     *
     * @return bool TRUE if the lock can be acquired, FALSE otherwise.
     */
    public function maybeLock()
    {
        // search inside lock table
        $c = new Criteria();
        $c->add(ArLockPeer::NAME, $this->name);
        $lock = ArLockPeer::doSelectOne($c);

        if (is_null($lock)) {
            // INSERT LOCK
            //
            $lock = new ArLock();
            $lock->setName($this->name);
            $lock->setTime(time());
            $lock->setInfo(getmypid());
            $lock->save();

            $this->removeLock = TRUE;

            return TRUE;
        } else {
            $pid = $lock->getInfo();
            // check if the LOCK is associated to a running process
            //
            $pids = explode(PHP_EOL, `ps -e | awk '{print $1}'`);
            if (in_array($pid, $pids)) {
                $removeLock = FALSE;
            } else {
                $removeLock = TRUE;
            }


            if ($removeLock) {
                // the process is killed, and the lock can be acquired
                //
                $lock->delete();
                return $this->maybeLock();
            } else {
                // the process is still running, lock can not be acquired
                //
                $this->removeLock = FALSE;
                return FALSE;
            }
        }
    }

    /**
     * Release the lock on the file.
     */
    public function unlock()
    {
        if ($this->removeLock == TRUE) {
            $c = new Criteria();
            $c->add(ArLockPeer::NAME, $this->name);
            $lock = ArLockPeer::doSelectOne($c);
            if (!is_null($lock)) {
                $lock->delete();
            }
            $this->removeLock = FALSE;
        }
    }

    /**
     * @param int $maxAge max allowed age of the tag, in unix timestamp format
     * @return bool TRUE if the tag is expired and in this case touch again the tag, FALSE otherwise.
     */
    public function maybeTouch($maxAge)
    {
        $c = new Criteria();
        $c->add(ArLockPeer::NAME, $this->name);
        $lock = ArLockPeer::doSelectOne($c);

        if (is_null($lock)) {
            $lock = new ArLock();
            $lock->setName($this->name);
            $lock->setTime(time());
            $lock->save();
            return TRUE;
        } else {
            $lastAge = strtotime($lock->getTime());
            if ($lastAge < $maxAge) {
                $lock->setTime(time());
                $lock->save();
                return TRUE;
            } else {
                return FALSE;
            }
        }
    }

    /**
     * @return string|null the info value of the tag associated to the mutex
     */
    public function getTagInfo()
    {
        $c = new Criteria();
        $c->add(ArLockPeer::NAME, $this->name);
        $lock = ArLockPeer::doSelectOne($c);

        if (is_null($lock)) {
            return NULL;
        } else {
            return $lock->getInfo();
        }
    }

    /**
     * @param string|null $info
     */
    public function setTagInfo($info)
    {
        $c = new Criteria();
        $c->add(ArLockPeer::NAME, $this->name);
        $lock = ArLockPeer::doSelectOne($c);

        if (!is_null($lock)) {
            $lock->setInfo($info);
            $lock->save();
        }
    }
}
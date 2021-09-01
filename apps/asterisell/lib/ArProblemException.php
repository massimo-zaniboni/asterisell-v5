<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

/**
 * A problem to signal into the ArNewProblem table and to signal as Exception in the PhpCode.
 *
 * NOTE: errors are added using a separate connection, that is distinct from the connection used for managing CDRs,
 * so it is not affected from application rollback:
 * - every connection can be associated only to 1 transaction
 * - there is the main PDO connection/transaction (MAIN)
 * - in case of severe errors the job can rollback the transaction on MAIN
 * - an error PDO connection (ERR) is used for signaling errors
 * - the error data written using ERR is not rollbacked if transaction on MAIN is rollbacked
 * - the UI shows the UI table, while the jobs add transactions to the job table
 * - at the end of the cron processor, the UI table is updated
 */
class ArProblemException extends Exception
{

    const showDebugMessages = false;

    static public $disableNotifications = 0;

    static protected $logConnection = null;

    static protected $singletone = null;

    static protected $lastProblemId = null;
    static protected $lastErrorDescription = null;
    static protected $lastErrorEffect = null;
    static protected $lastErrorSolution = null;
    static protected $lastProblemTypeId = null;
    static protected $lastProblemDomainId = null;
    static protected $lastProblemResponsibleId = null;
   
    /**
     * Use a separate connection, because notification logic, is different from logic used for
     * rollbacking CDR processing (notifications must be not rollbacked).
     *
     * @static
     * @return PropelPDO dedicated connection for writing errors and notification to the administrators.
     */
    static public function getLogConnection()
    {
        if (is_null(self::$logConnection)) {
            $databaseParams = sfYaml::load(file_get_contents(sfConfig::get('sf_root_dir') . DIRECTORY_SEPARATOR . 'config' . DIRECTORY_SEPARATOR . 'databases.yml'));
            $params = $databaseParams['all']['propel']['param'];
            self::$logConnection = new PropelPDO(
                $params['dsn'],
                $params['username'], $params['password'],
                array(
                    PDO::ATTR_PERSISTENT => false,
                    PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
                    PDO::ATTR_AUTOCOMMIT => true
                ));
        }

        return self::$logConnection;
    }

    /**
     * Allow to add errors inside a transaction, in order to show them at the end of job processing.
     * The transaction is "simulated" using two error tables, because there were errors using
     * transactions on error-log connection.
     *
     * @return void
     */
    static public function beginLogTransaction()
    {
        try {
            // Use the normal DB connection for updating this field
            $deleteErrorTable = FixedJobProcessor::getCleanErrorTable(self::getLogConnection());
            if ($deleteErrorTable == 1) {
                // switch from scheduled, to in action
                FixedJobProcessor::setCleanErrorTable(self::getLogConnection(), 2);

                // use ar_new_problem and ar_current_problem as a poor-man transaction management:
                // they will be switched at the end of job processing
                $str = 'DELETE FROM ar_new_problem WHERE NOT ar_problem_type_id = ' . ArProblemType::TYPE_INTERNAL_LOG;
                self::getLogConnection()->exec($str);
            }
        } catch (Exception $e) {
            $problemDuplicationKey = "FixedJobs Processor begin - ArProblemException::beginLogTransaction() does not support transactions - exception .";
            $problemDescription = "Internal problem: ArProblemException::beginLogTransaction() does not support transactions. Exception: " . $e->getMessage();
            $problemEffect = "Error reporting is not optimal.";
            $problemProposedSolution = "Contact the assistance.";
            ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_WARNING,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
        }
    }

    /**
     * The transaction is "simulated" using two error tables, because there were errors using
     * transactions on error-log connection.
     */
    static public function commitLogTransaction()
    {
        $conn = self::getLogConnection();
        // DEV-NOTE: does not use a transaction, because in this case
        // the "REPEATABLE READ" transaction isolation level
        // does not guarantee the correct semantic in case of intervalead update/insert/delete statements

        $fields = implode(',', ArNewProblemPeer::getFieldNames(BasePeer::TYPE_FIELDNAME));
        try {
            $deleteErrorTable = FixedJobProcessor::getCleanErrorTable($conn);
            if ($deleteErrorTable == 2) {
                FixedJobProcessor::setCleanErrorTable($conn, 0);
            }

            self::updateSentToAdminStateInNewProblems($conn);

            $str = 'DELETE FROM ar_current_problem WHERE TRUE';
            $conn->exec($str);

            $str = "INSERT INTO ar_current_problem($fields) SELECT $fields FROM ar_new_problem";
            $conn->exec($str);

        } catch (Exception $e) {
            $problemDuplicationKey = "FixedJobs Processor - ArProblemException::beginLogTransaction() does not support transactions - exception .";
            $problemDescription = "Internal problem: ArProblemException::commitLogTransaction() does not support transactions. Exception: " . $e->getMessage();
            $problemEffect = "Error reporting is not optimal.";
            $problemProposedSolution = "Contact the assistance.";
            ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_WARNING,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
        }
    }

    /**
     * Update the `signaled_to_admin` status of errors.
     * @param PDO $conn
     */
    static public function updateSentToAdminStateInNewProblems($conn)
    {
        // a problem already signaled to the admin in the past, remain signaled to admin also on ar_new_problem table.
        $cmd = 'UPDATE ar_new_problem n
                JOIN ar_current_problem c
                ON n.duplication_key = c.duplication_key
                SET n.signaled_to_admin = 1
                WHERE c.signaled_to_admin = 1;';

        $conn->exec($cmd);
    }

    /**
     * Call this method if the notification to admin must be temporary
     * disabled. In this case all new problems will be considered
     * as already sent. This allows to work-online with the program,
     * without sending many warning emails.
     */
    public static function disableNotificationsToAdmin()
    {
        ArProblemException::$disableNotifications = 1;
    }

    public function __construct()
    {
        parent::__construct('Asterisell Exception');
    }

    /**
     * @static
     * @param int $severityLevelId error type, use one of ArProblemType::TYPE_ constants.
     * @param int $problemDomainId ArProblemDomain constant
     * @param int|null $responsibleId null for using the default responsible, ArProblemResponsible constant
     * @param string $key
     * @param string $description
     * @param string $effect
     * @param string $solution
     * @param string $forceDupKey
     * @return ArProblemException
     */
    static public function createWithoutGarbageCollection($severityLevelId, $problemDomainId, $responsibleId, $key, $description, $effect, $solution, $forceDupKey = null)
    {
        return self::internal_create($severityLevelId, $problemDomainId, $responsibleId, $key, null, null, null, $description, $effect, $solution, $forceDupKey);
    }

    /**
     * @param Exception $e
     * @param string $keyPrefix
     * @param string $msgPrefix
     * @param string|null $msgEffect
     * @param string|null $msgSolution
     * @return ArProblemException
     */
    static public function createFromGenericExceptionWithoutGarbageCollection(Exception $e, $keyPrefix, $msgPrefix, $msgEffect = null, $msgSolution = null)
    {
        return self::internal_createFromGenericException($e, ArProblemType::TYPE_ERROR, $keyPrefix, null, null, null, false, $msgPrefix, $msgEffect, $msgSolution);
    }

    /**
     * The error key is used also for garbage collecting the errors.
     * In this way only still relevant errors are maintained in the error table.
     *
     * @static
     * @param int $severityId error type, use one of ArProblemType::TYPE_ constants.
     * @param int $problemDomainId ArProblemDomain constant
     * @param int|null $responsibleId null for using the default responsible, ArProblemResponsible constant
     * @param string $uniqueKey
     * @param string $garbageKey
     * @param int|null $from
     * @param int|null $to
     * @param string $description
     * @param string $effect
     * @param string $solution
     * @param string $forceDupKey
     * @return ArProblemException
     */
    static public function createWithGarbageCollection($severityId, $problemDomainId, $responsibleId, $uniqueKey, $garbageKey, $from, $to, $description, $effect, $solution, $forceDupKey = null)
    {
        return self::internal_create($severityId, $problemDomainId, $responsibleId, $uniqueKey, $garbageKey, $from, $to, $description, $effect, $solution, $forceDupKey);
    }

    /**
     * @param Exception $e
     * @param int $severityLevelId one of ArProblemType::TYPE_ const values
     * @param string $uniqueKey
     * @param string $garbageKey
     * @param int|null $from
     * @param int|null $to
     * @param string $msgPrefix
     * @param string|null $msgEffect
     * @param string|null $msgSolution
     * @return ArProblemException
     */
    static public function createFromGenericExceptionWithGarbageCollection(Exception $e, $severityLevelId, $uniqueKey, $garbageKey, $from, $to, $msgPrefix, $msgEffect = null, $msgSolution = null)
    {
        return self::internal_createFromGenericException($e, $severityLevelId, $uniqueKey, $garbageKey, $from, $to, true, $msgPrefix, $msgEffect, $msgSolution);
    }

    /**
     * @return int
     */
    static public function getLastArProblemId()
    {
        return self::$lastProblemId;
    }

    /**
     * @return ArNewProblem
     */
    static public function getLastArProblem()
    {
        return ArNewProblemPeer::retrieveByPK(self::$lastProblemId);
    }

    /**
     * The message associated to the last generated error.
     *
     * @return string
     */
    static public function getLastErrorDescription()
    {
        $s = self::$lastErrorDescription;
        if (is_null($s)) {
            return '';
        } else {
            return $s;
        }
    }

    /**
     * The message associated to the last generated error.
     *
     * @return string
     */
    static public function getLastErrorEffect()
    {
        $s = self::$lastErrorEffect;
        if (is_null($s)) {
            return '';
        } else {
            return $s;
        }
    }

    /**
     * The message associated to the last generated error.
     *
     * @return string
     */
    static public function getLastErrorSolution()
    {
        $s = self::$lastErrorSolution;
        if (is_null($s)) {
            return '';
        } else {
            return $s;
        }
    }

    /**
     * @static
     * @param int $typeId error type, use one of ArProblemType::TYPE_ constants.
     * @param int $problemDomainId
     * @param int|null $responsibleId
     * @param string $key
     * @param string|null $garbageCollectionKey null for no support of garbage collection
     * @param int|null $from
     * @param int|null $to
     * @param string $description
     * @param string $effect
     * @param string $solution
     * @param string|null $forceDupKey
     * @return ArProblemException
     * @throws ArProblemException
     */
    static protected function internal_create($typeId, $problemDomainId, $responsibleId, $key, $garbageCollectionKey, $from, $to, $description, $effect, $solution, $forceDupKey = null)
    {
        if (is_null($responsibleId)) {
            $responsibleId = ArProblemDefaultResponsiblePeer::getDefaultResponsibleForErrorDomain($problemDomainId);
        }

        if (is_null($from)) {
            $from = 0;
        }

        if (is_null($to)) {
            $to = time();
        }

        if (ArProblemException::$disableNotifications == 1) {
            $signaledToAdmin = 1;
            // add the problem, but does not advise admin of the problem via mail
        } else {
            $signaledToAdmin = 0;
        }

        // NOTE: use a md5 for speed reason, but primary for using compact links to errors in the user interface,
        // that do not depends from IDs, because in some advanced processing (or transactions) they can change.
        if (is_null($forceDupKey)) {
            $dupKey = md5($key);
        } else {
            $dupKey = $forceDupKey;
        }

        $conn = self::getLogConnection();

        $query = 'INSERT INTO ar_new_problem(
          ar_problem_type_id,
          ar_problem_domain_id,
          ar_problem_responsible_id,
          created_at,
          duplication_key,
          garbage_collection_key,
          garbage_collection_from,
          garbage_collection_to,
          description,
          effect,
          proposed_solution,
          signaled_to_admin)
          VALUES(?, ?, ?, NOW(), ?, ?, ?, ?, ?, ?, ?, ?)
          ON DUPLICATE KEY
          UPDATE     garbage_collection_from = LEAST(garbage_collection_from, VALUES(garbage_collection_from))
                   , garbage_collection_to = GREATEST(garbage_collection_to, VALUES(garbage_collection_to))
                   , ar_problem_type_id = VALUES(ar_problem_type_id)
                   , ar_problem_domain_id  = VALUES(ar_problem_domain_id)
                   , ar_problem_responsible_id  = VALUES(ar_problem_responsible_id)
                   , created_at = VALUES(created_at)
                   , garbage_collection_key = VALUES(garbage_collection_key )
                   , description = VALUES(description )
                   , effect = VALUES(effect)
                   , proposed_solution = VALUES(proposed_solution)
                   , signaled_to_admin  = (VALUES(signaled_to_admin) OR signaled_to_admin)
          ';
        $stmt = $conn->prepare($query);
        $stmt->execute(array($typeId, $problemDomainId, $responsibleId, $dupKey, $garbageCollectionKey, fromUnixTimestampToMySQLTimestamp($from), fromUnixTimestampToMySQLTimestamp($to), $description, $effect, $solution, (int)$signaledToAdmin));
        $problemId = $conn->lastInsertId();
        $stmt->closeCursor();

        if (is_null(self::$singletone)) {
            self::$singletone = new ArProblemException();
        }

        self::$lastProblemId = $problemId;
        self::$lastErrorDescription = $description;
        self::$lastErrorEffect = $effect;
        self::$lastErrorSolution = $solution;
        self::$lastProblemDomainId = $problemDomainId;
        self::$lastProblemResponsibleId = $responsibleId;
        self::$lastProblemTypeId = $typeId;

        return self::$singletone;
    }

    /**
     * @param Exception $e
     * @param int $typeId one of ArProblemType::TYPE_ const values
     * @param string $keyPrefix
     * @param string $garbageKey
     * @param int|null $from
     * @param int|null $to
     * @param bool $useGarbageCollection ,
     * @param string $msgPrefix
     * @param string|null $msgEffect
     * @param string|null $msgSolution
     * @return ArProblemException
     */
    static public function internal_createFromGenericException(Exception $e, $typeId, $keyPrefix, $garbageKey, $from, $to, $useGarbageCollection, $msgPrefix, $msgEffect = null, $msgSolution = null)
    {
        $key = "$keyPrefix - exception - " . rand();
        $description = $msgPrefix . ' ' . self::describeGenericException($e);

        if (is_null($msgEffect)) {
            $effect = "This error prevent the execution of the current job.";
        } else {
            $effect = $msgEffect;
        }

        if (is_null($msgSolution)) {
            $solution = "If the problem persist, contact the assistance.";
        } else {
            $solution = $msgSolution;
        }

        if (!$useGarbageCollection) {
            $garbageKey = null;
        }

        return self::internal_create($typeId, ArProblemDomain::APPLICATION, null, $key, $garbageKey, $from, $to, $description, $effect, $solution);
    }

    static public function describeGenericException(Exception $e)
    {
        return "Exception error message is: " . $e->getMessage() . ". Stack trace: " . $e->getTraceAsString();
    }

    /**
     * Delete the errors from problem table that can be replaced from this new garbage collected
     * stream of errors, regarding the CDRs in the specified time frame.
     *
     * @require this function is called inside the try catch block of the transaction of the user
     *
     * @static
     * @param string|null $garbageCollectionKey null for no support of garbage collection
     * @param int|null $from
     * @param int|null $to
     * @return void
     * @return ArProblemException
     */
    static public function garbageCollect($garbageCollectionKey, $from, $to)
    {
        $conn = self::getLogConnection();

        $params = array();
        $cmd = 'DELETE FROM ar_new_problem WHERE garbage_collection_key = ? ';
        $params[] = $garbageCollectionKey;

        if (!is_null($from)) {
            $cmd .= ' AND garbage_collection_from >= ? ';
            $params[] = fromUnixTimestampToMySQLTimestamp($from);
        }

        if (!is_null($to)) {
            $cmd .= ' AND garbage_collection_to <= ? ';
            $params[] = fromUnixTimestampToMySQLTimestamp($to);

        }

        $stmt = $conn->prepare($cmd);
        $stmt->execute($params);
        $stmt->closeCursor();

        if (self::showDebugMessages) {
            echo "\n     garbage error messages of type \"$garbageCollectionKey\", from " . fromUnixTimestampToMySQLTimestamp($from) . ' to ' . fromUnixTimestampToMySQLTimestamp($to);
        }
    }
}

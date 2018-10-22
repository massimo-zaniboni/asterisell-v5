<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

class LoadWorldTelephonePrefixesFromCSVFile extends InitTelephonePrefixes
{
    public function process()
    {
        $count = $this->loadAllPrefixes(false);
        return "Loaded $count prefixes.";
    }

    /**
     * @param bool $merge
     * @return int
     */
    public function loadAllPrefixes($merge)
    {
        return $this->loadPrefixes(getAsterisellCompleteRootDirectory() . "/scripts/world_prefix_table.csv", $merge);
    }

    /**
     * @param string $filename
     * @param bool $merge
     * @return int loaded prefixes
     * @throws Exception
     * @throws ArProblemException
     */
    public function loadPrefixes($filename, $merge)
    {
        $handle = fopen($filename, 'r');
        if ($handle === FALSE) {
            $problemDuplicationKey = "init world prefixes " . get_class($this);
            $problemDescription = "In class ' . get_class($this) . ' it was not possible loading world prefixes file \"$filename\".";
            $problemProposedSolution = "This is an error in the application. Contact the assistance.";
            $problemEffect = "The application have no world telephone prefixes, and it can not rate the calls.";
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_CRITICAL,
                ArProblemDomain::CONFIGURATIONS,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }

        return $this->loadPrefixesFromHandle($handle, $merge);
    }

    /**
     * @param resource $handle
     * @param bool $merge
     * @return int
     * @throws Exception
     * @throws ArProblemException
     */
    public function loadPrefixesFromHandle($handle, $merge)
    {
        $conn = Propel::getConnection();
        $conn->beginTransaction();

        try {
            $count = 0;

            while (($data = fgetcsv($handle, 50000, ",")) !== false) {
                $count++;

                $prefix = '';
                $nation = '';
                $type = '';
                $operator = '';

                try {

                    $prefix = trim($data[0]);
                    $nation = trim($data[1]);
                    $type = trim($data[2]);
                    $operator = trim($data[3]);

                    if ($merge) {
                        $this->mergeCompletePrefix($prefix, $nation, $type, $operator);
                    } else {
                        $this->createCompletePrefix($prefix, $nation, $type, $operator);
                    }
                } catch (Exception $e) {
                    $problemDuplicationKey = "init world prefixes " . get_class($this);
                    $problemDescription = "In job " . get_class($this) . " it was not possible loading prefix \"$prefix\", with location \"$nation\", and connection type \"$type\", at line $count. Error message: " . $e->getMessage();
                    $problemProposedSolution = "This is an error in the application or in the format of the CSV file. In case contact the assistance.";
                    $problemEffect = "The application can not load the prefixes.";
                    $p = ArProblemException::createWithoutGarbageCollection(
                        ArProblemType::TYPE_ERROR,
                        ArProblemDomain::CONFIGURATIONS,
                        null,
                        $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                    throw($p);
                }
            }

            $this->commitTransactionOrSignalProblem($conn);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        }
        return $count;
    }

    /**
     * @param string $prefix
     * @param string $nation
     * @param string $type
     * @param string $operator
     * @return void
     */
    protected function createCompletePrefix($prefix, $nation, $type, $operator)
    {
        $r = new ArTelephonePrefix();
        $r->setPrefix($prefix);
        $r->setName($operator);
        $r->setGeographicLocation($nation);
        $r->setOperatorType($type);
        $r->save();
    }

    /**
     * @param string $prefix
     * @param string $nation
     * @param string $type
     * @param string $operator
     * @return void
     */
    protected function mergeCompletePrefix($prefix, $nation, $type, $operator)
    {

        $c = new Criteria();
        $c->add(ArTelephonePrefixPeer::PREFIX, $prefix, Criteria::EQUAL);
        $r = ArTelephonePrefixPeer::doSelectOne($c);

        if (is_null($r)) {
            $r = new ArTelephonePrefix();
        }
        $r->setPrefix($prefix);
        $r->setName($operator);
        $r->setGeographicLocation($nation);
        $r->setOperatorType($type);
        $r->setMatchOnlyNumbersWithNDigits(null);
        $r->save();
    }
}

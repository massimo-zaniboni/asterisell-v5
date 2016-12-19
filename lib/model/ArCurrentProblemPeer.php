<?php

require 'lib/model/om/BaseArCurrentProblemPeer.php';


class ArCurrentProblemPeer extends BaseArCurrentProblemPeer {


    /**
     * @static
     * @param string $internalName
     * @return ArCurrentProblem|null
     */
    public static function retrieveByDuplicationKey($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArCurrentProblemPeer::DUPLICATION_KEY, $internalName);
        return ArCurrentProblemPeer::doSelectOne($criteria);
    }

    /**
     * @param PDO|null $conn
     * @param bool $useNewProblemTable true for using instead of the current table, the new table
     * @return array an array with a textual description. Each array row contains info about
     * list($severityLevelName, $numberOfProblems, $responsibleName, $problemDomainName, $severityLevelId, $responsibleId, $domainId)
     * ordered by severity level.
     */
    static public function getProblemSummaryByType($conn = null, $useNewProblemTable = false) {

        if (is_null($conn)) {
            $conn = Propel::getConnection();
        }

        if ($useNewProblemTable) {
            $table = 'ar_new_problem';
        } else {
            $table = 'ar_current_problem';
        }

        $stm = $conn->prepare('
SELECT COUNT(t.duplication_key),
  ar_problem_type_id,
  ar_problem_responsible_id,
  ar_problem_domain_id,
  ar_problem_type.name,
  ar_problem_responsible.name,
  ar_problem_domain.name
FROM ' . $table . ' AS t
  INNER JOIN  ar_problem_type ON t.ar_problem_type_id = ar_problem_type.id
  INNER JOIN  ar_problem_domain ON t.ar_problem_domain_id = ar_problem_domain.id
  INNER JOIN  ar_problem_responsible ON t.ar_problem_responsible_id = ar_problem_responsible.id
WHERE NOT t.ar_problem_type_id = ?
GROUP BY ar_problem_type_id, ar_problem_responsible_id, ar_problem_domain_id
ORDER BY ar_problem_type_id DESC;
        ');
        $stm->execute(array(ArProblemType::TYPE_INTERNAL_LOG));

        $r = array();
        while ((($rs = $stm->fetch(PDO::FETCH_NUM)) !== false)) {
            $row = array();
            $row[0] = $rs[4];
            $row[1] = $rs[0];
            $row[2] = $rs[5];
            $row[3] = $rs[6];
            $row[4] = $rs[1];
            $row[5] = $rs[2];
            $row[6] = $rs[3];
            $r[] = $row;
        }
        $stm->closeCursor();

        return $r;
    }

} // ArCurrentProblemPeer

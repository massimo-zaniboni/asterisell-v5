<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


class CheckForMalformedErrorsNotSentToUsers extends FixedJobProcessor
{

    public function process()
    {
        $conn = Propel::getConnection();

        $stm = $conn->prepare('
SELECT COUNT(pp.duplication_key)
FROM ar_current_problem AS pp
WHERE pp.duplication_key NOT IN (
        SELECT ar_current_problem.duplication_key
        FROM ar_current_problem
        INNER JOIN  ar_problem_type ON ar_current_problem.ar_problem_type_id = ar_problem_type.id
        INNER JOIN  ar_problem_domain ON ar_current_problem.ar_problem_domain_id = ar_problem_domain.id
        INNER JOIN  ar_problem_responsible ON ar_current_problem.ar_problem_responsible_id = ar_problem_responsible.id
);
        ');
        $stm->execute();

        while (($rs = $stm->fetch(PDO::FETCH_NUM)) !== false) {

            $count = intval($rs[0]);

            if ($count > 0) {
                $problemDuplicationKey = get_class($this);
                $problemDescription = 'There are ' . $count . ' error messages that have not a correct format.';
                $problemEffect = 'These error messages are not displayed, and sent to administrators, and the related problems signaled by these error messages, can be left unnoticed.';
                $problemProposedSolution = 'This is an error in application code. Contact the assistance.';
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution
                );
            }
        }
        $stm->closeCursor();
    }
}

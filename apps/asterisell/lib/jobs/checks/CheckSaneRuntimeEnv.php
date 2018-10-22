<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Check if there is something of strange in the runtime/operational env.
 */
class CheckSaneRuntimeEnv extends FixedJobProcessor
{

    public function process()
    {
        $garbageKey = get_class($this);
        ArProblemException::garbageCollect($garbageKey, null, null);

        $conn = Propel::getConnection();

        // Check if there are too much ported telephone number

        $query = "SELECT count(*) FROM ar_number_portability";
        $stm = $conn->prepare($query);
        $stm->execute();
        $num = 0;
        while ($rs = $stm->fetch(PDO::FETCH_NUM)) {
            $num = $rs[0];
        }
        $stm->closeCursor();
        $limit = 10000000;
        if ($num > $limit) {
            $problemDuplicationKey = get_class($this) . " - too much ported telephone numbers";
            $problemDescription = 'There are ' . $num . ' ported telephone numbers, and this is an unexpected high number. There can be an error in the importing procedure, or in the input data files.';
            $problemEffect = "The application can be slow, or calls are rated wrongly because many of these numbers are not really ported.";
            $problemProposedSolution = 'Check if all these numbers are really to import as ported telephone numbers.';
            ArProblemException::createWithGarbageCollection(
                ArProblemType::TYPE_WARNING
                , ArProblemDomain::RATES
                , null
                , $problemDuplicationKey
                , $garbageKey
                , null
                , null
                , $problemDescription
                , $problemEffect
                , $problemProposedSolution);

        }


        return '';
    }
}

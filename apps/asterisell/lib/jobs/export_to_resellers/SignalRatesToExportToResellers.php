<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * This job generate an event for each Reseller registered for receiving shared Rates.
 * The rates will be phisically exported from job `ExportCDRSToReseller`.
 */
class SignalRatesToExportToResellers extends FixedJobProcessor
{

    public function process() {
        $conn = Propel::getConnection();
        $conn->beginTransaction();

        try {
            // Signal the rates to send to resellers
            // NOTE: take care of modifying rates only if really needed, because otherwise too much triggers are activated.
            $query = 'UPDATE ar_rate_shared_with_reseller
                      JOIN   ar_rate ON ar_rate_shared_with_reseller.ar_rate_id = ar_rate.id
                      SET    ar_rate_shared_with_reseller.is_exported = 0
                      WHERE  ar_rate.is_exported_to_resellers = 0
                      AND    ar_rate_shared_with_reseller.is_exported <> 0;';

            $stm = $conn->prepare($query);
            $stm->execute();
            $stm->closeCursor();

            // Now can signal the rates as "virtually" exported,
            // because the actions will be done on the reseller level.
            // In MySQL by default DML does not follow the repeteable read isolation level,
            // so I punt in join with the modified rates, for not deleting rates inserted
            // from the ADMIN while this job is running

            $query = 'UPDATE ar_rate
                      JOIN   ar_rate_shared_with_reseller
                      ON     ar_rate_shared_with_reseller.ar_rate_id = ar_rate.id
                      SET    ar_rate.is_exported_to_resellers = 1
                      WHERE  ar_rate_shared_with_reseller.is_exported = 0
                      AND    ar_rate.is_exported_to_resellers <> 1;';

            $stm = $conn->prepare($query);
            $stm->execute();
            $stm->closeCursor();

            $this->commitTransactionOrSignalProblem($conn);

        } catch(Exception $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        }

        return '';

    }
}

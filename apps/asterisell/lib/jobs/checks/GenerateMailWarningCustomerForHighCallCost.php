<?php

/* $LICENSE 2009, 2010, 2015:
 *
 * Copyright (C) 2009, 2010, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Compose and send an Email message warning the customer for high call costs.
 */
class GenerateMailWarningCustomerForHighCallCost extends JobProcessor
{


    public function processEvent(JobData $jobData, $parentJobId)
    {

        if (!($jobData instanceof CustomerHasHighCallCostEvent)) {
            return null;
        }

        /**
         * @var CustomerHasHighCallCostEvent $jobData
         */

        $party = ArPartyPeer::retrieveByPK($jobData->arPartyId);
        $params = ArParamsPeer::getDefaultParams();

        if (is_null($params)) {
            return null;
        }

        if (isEmptyOrNull($party->getEmail())) {
            return null;
        }

        if ($jobData->method === '30') {
            $timeframe = date("c", strtotime("-30 day"));
            $timeframeDescription = "in the last 30 days";
            $timeframeDescriptionIT = "negli ultimi 30 giorni";
        } else {
            $timeframe = date('Y') . '-' . date('m') . '-' . '01';
            $timeframeDescription = "on current month";
            $timeframeDescriptionIT = "nel mese corrente";
        }

        $providerName = $params->getServiceName();
        $culture = sfConfig::get('app_culture');
        $currency = sfConfig::get('app_currency');

        $eol = "\r\n";
        if ($culture === 'it_IT') {
            $r = 'Spettabile ' . $party->getFullName() . ', ' . $eol . $eol . 'la informiamo che il costo delle telefonate ' . $timeframeDescriptionIT . ' e` stato di ' . from_db_decimal_to_pdf_txt_decimal($jobData->effectiveCost) . ' e potrebbe essere sospetto dato che ha superato il limite preimpostato di ' . from_db_decimal_to_pdf_txt_decimal($jobData->costLimit);

            $r .= " . La invitiamo a consultare il report delle telefonate all'indirizzo web " . $params->getServiceProviderWebsite() . " e di contattarci all'indirizzo " . $params->getServiceProviderEmail();

            $r .= ' in caso di anomalie.' . $eol . $eol . "Grazie per l'attenzione, " . $eol . $providerName;

            $subject = $providerName . " - Superamento limite costo telefonate";

        } else {
            $r = $party->getFullName() . ', ' . $timeframeDescription . ' your call cost was ' . from_db_decimal_to_pdf_txt_decimal($jobData->effectiveCost) . ' and it is higher than the limit of ' . from_db_decimal_to_pdf_txt_decimal($jobData->costLimit) . '. You can inspect your call report at ' . $params->getServiceProviderWebsite() . ' If there are anomalies you should inform ours at ' . $params->getServiceProviderEmail() . '.' . $eol . 'Best regards,' . $eol . $providerName;

            $subject = "$providerName - suspicious call cost";
        }

        $this->sendWarningEmail($jobData, $subject, $r);

        return 'Sent email to party ' . $party->getFullName();
    }

    /**
     * Send the email to customers.
     *
     * @param CustomerHasHighCallCostEvent $d
     * @param string $subject
     * @param string $content
     * @return void
     */
    protected function sendWarningEmail($d, $subject, $content)
    {
        $emails = $this->getEmailsForWarningsAboutAccount($d->unitId);

        if (count($emails) > 0) {
            list($mainEmail, $mainName) = array_shift($emails);
            $isOk = $this->sendEmail($mainEmail, $mainName, $emails, $subject, $content);
            if ($isOk) {
                $party = ArPartyPeer::retrieveByPk($d->arPartyId);
                $party->setLastEmailAdviseForMaxLimit30(date("c"));
                $party->save();
            }
        }
    }

}

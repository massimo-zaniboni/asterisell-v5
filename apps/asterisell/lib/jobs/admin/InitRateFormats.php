<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell', 'Markdown'));

/**
 * Add/update info about the rates to use.
 */
class InitRateFormats extends AdminJobProcessor
{

    /**
     * IMPORTANT:
     * - you can add constants to this list, and the new descriptions will be loaded with next makeActivate
     * - you can not change the already defined constants, because the already installed instances can not upgrade the old formats
     * - in case write some explicit upgrade procedure converting the changed names to the new ones
     */

    const REFERENCE_RATE_PLAN_SPECIFICATION = 'rate-plan-specification';
    const REFERENCE_CSV_WITH_HEADER_3COL = 'csv-header-3col';
    const REFERENCE_CSV_WITH_HEADER_3COL_LAST_DESCR = 'csv-header-3col-last-descr';
    const REFERENCE_CSV_WITH_HEADER_3COL_IT = 'csv-header-3col-italian';
    const REFERENCE_CSV_WITH_HEADER_4COL = 'csv-header-4col';
    const REFERENCE_CSV_WITH_HEADER_4COL_COST_ON_CALL = 'csv-header-4col-costOnCall';
    const REFERENCE_CSV_WITH_HEADER_4COL_RATING_CODE_COST_ON_CALL = 'csv-header-4col-rating-code-costOnCall';
    const REFERENCE_CSV_WITH_HEADER_5COL_COST_ON_CALL = 'csv-header-5col-costOnCall';
    const REFERENCE_CSV_WITH_HEADER_6COL_COST_ON_CALL = 'csv-header-6col-costOnCall';
    const REFERENCE_CSV_WITH_HEADER_7COL_TWT = 'csv-twt-header-7col';
    const REFERENCE_CSV_WITH_HEADER_7COL_TWT_IT = 'csv-twt-header-7col-italian';
    const REFERENCE_CSV_WITH_NO_HEADER_7COL_TWT = 'csv-twt-no-header-7col';
    const REFERENCE_CSV_WITH_HEADER_5COL_TWT_NNG = 'csv-twt-nng-5col';
    const REFERENCE_CSV_WITH_HEADER_5COL_TWT = 'csv-twt-header-5col';
    const REFERENCE_CSV_WITH_NO_HEADER_5COL_TWT = 'csv-twt-no-header-5col';
    const REFERENCE_CSV_WITH_HEADER_9COL_GAMMA = 'csv-gamma-header-9col';
    const REFERENCE_CSV_WITH_HEADER_6COL_GAMMA_ITEM_RENTAL = 'csv-gamma-item-rental-6col';
    const REFERENCE_CSV_WITH_HEADER_3COL_PDR_IT = 'csv-header-3col-pref-descr-rate-it';
    const REFERENCE_CSV_WITH_HEADER_3COL_PDR = 'csv-header-3col-pref-descr-rate';
    const REFERENCE_CSV_DIGITEL_NNG = 'csv-digitel-nng';
    const REFERENCE_CSV_ECN = 'csv-ecn';

    public function isCDRTableModified()
    {
        return false;
    }

    /**
     * Function to call for creating/upgrading rate formats.
     * @param string $internalName
     * @return ArRateFormat an existing or new rate format.
     */
    protected function createRateFormat($internalName) {

        $r = ArRateFormatPeer::retrieveByInternalName($internalName);
        if (is_null($r)) {
            $r = new ArRateFormat();
            $r->setInternalName($internalName);
        }

        return $r;
    }

    /**
     * Delete (maybe) a rate format.
     * @param string $internaName
     */
    protected function deleteRateFormat($internaName) {
        $r = ArRateFormatPeer::retrieveByInternalName($internaName);
        if (!is_null($r)) {
            $r->delete();
        }
    }

    public function process()
    {
        $r = $this->createRateFormat(self::REFERENCE_RATE_PLAN_SPECIFICATION);
        $r->setOrderName('00');
        $r->setShortDescription("Complete rate plan, using nested rules");
        $r->setDetailedDescription(fromMarkdownToHtml('

See application manual for the description of the rate plan specification language, at https://www.asterisell.com

An example of rate specification:

    #
    class {
      id: outgoing
      match-call-direction: outgoing

      rate {
        id: free-emergency-telephone-numbers
        match-telephone-number: 118,113,11X
      }

      rate {
        id: default

        match-price-category: normal
        set-cost-on-call: 0.05
        external-rate {
          id: csv-1
          use: csv-1
        }
      }
    }

    rate {
      id: free-incoming
      match-call-direction: incoming
      set-cost-on-call: 0
    }

        '));

        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_3COL);
        $r->setOrderName('30');
        $r->setShortDescription("CSV file, with header row, description, telephone prefix, and cost by minute columns");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"DESTINATION","PREFIX","RATE"
"Afghanistan",93,0.1175
"Afghanistan Mobile",937,0.1175
"Afghanistan Mobile - Etisalat",9378,0.1175
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_3COL_LAST_DESCR);
        $r->setOrderName('32');
        $r->setShortDescription("CSV file, with header row, telephone prefix, cost by minute, and description columns");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"PREFIX","RATE","DESTINATION"
937,0.1175,"Afghanistan Mobile"
        ') . '</pre>');

        $r->save();
        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_3COL_IT);
        $r->setOrderName('30-IT');
        $r->setShortDescription("CSV file, with header row, description, telephone prefix, and cost by minute columns, using \",\" as decimal separator.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"DESTINATION","PREFIX","RATE"
"Afghanistan",93,"0,1175"
"Afghanistan Mobile",937,"0,1175"
"Afghanistan Mobile - Etisalat",9378,"0,1175"
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_4COL);
        $r->setOrderName('40');
        $r->setShortDescription("CSV file, with header row, 2 columns for descriptions, telephone prefix, cost by minute, using \".\" as decimal separator.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"LOCATION","OPERATOR","PREFIX","RATE"
"Afghanistan","Telecom",93,"0.1175","0.01"
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_4COL_COST_ON_CALL);
        $r->setOrderName('40-CALL');
        $r->setShortDescription("CSV file, with header row, 1 columns for descriptions, telephone prefix, cost by minute, and cost on call columns, using \".\" as decimal separator.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"LOCATION","OPERATOR","PREFIX","RATE BY MINUTE","COST ON CALL"
"Afghanistan","Telecom",93,"0.1175","0.01"
        ') . '</pre>');
        $r->save();


        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_5COL_COST_ON_CALL);
        $r->setOrderName('50-CALL');
        $r->setShortDescription("CSV file, with header row, 2 columns for descriptions, telephone prefix, cost by minute, and cost on call columns, using \".\" as decimal separator.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"LOCATION","OPERATOR","PREFIX","RATE BY MINUTE","COST ON CALL"
"Afghanistan","Telecom",93,"0.1175","0.01","0.01"
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_6COL_COST_ON_CALL);
        $r->setOrderName('60-CALL');
        $r->setShortDescription("CSV file, with header row, 3 columns for descriptions, telephone prefix, cost by minute, and cost on call columns, using \".\" as decimal separator.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"LOCATION","OPERATOR","TYPE","PREFIX","RATE BY MINUTE","COST ON CALL"
"Afghanistan","Telecom","Fixed Line",93,"0.1175","0.01","0.01"
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_7COL_TWT);
        $r->setOrderName('TWT-70');
        $r->setShortDescription("CSV file, in TWT format.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
Destinations,Code,Peak,Off peak,Effective,Route Type,Comment
Afghanistan,93,0.1230,0.1230,05/06/2014,-,==
Afghanistan Mobile,937,0.1391,0.1391,05/06/2014,-,==
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_7COL_TWT_IT);
        $r->setOrderName('TWT-71');
        $r->setShortDescription("CSV file, in TWT format, with italian cost format");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
Destinations;Code;Peak;Off peak,Effective;Route Type;Comment
Afghanistan;93;0,1230;0,1230;05/06/2014;-;==
Afghanistan Mobile;937;0,1391;0,1391;05/06/2014;-;==
        ') . '</pre>');
        $r->save();


        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_NO_HEADER_7COL_TWT);
        $r->setOrderName('TWT-72');
        $r->setShortDescription("CSV file, in TWT format.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
Afghanistan,93,0.1230,0.1230,05/06/2014,-,==
Afghanistan Mobile,937,0.1391,0.1391,05/06/2014,-,==
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_5COL_TWT_NNG);
        $r->setOrderName('TWT-50');
        $r->setShortDescription("CSV file, in TWT format, containing NNG rates.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
Operatore,Localita,Numero,Costo fascia intera,Costo alla risposta
Tiscali,Italia,39178221,0.0826,0.0656
Eutelia,Italia,3917824,0.0491,0.1033
Tiscali,Italia,39178275,0.0826,0.0656
        ') . '</pre>

<p>The telephone prefix is transformed to something like "Eutelia-3917824", so the CDR to rate must use this telephone number format.
        ');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_5COL_TWT);
        $r->setOrderName('TWT-50_PREFIX');
        $r->setShortDescription("CSV file, in TWT format, with telephone prefix, description, rate by minute, and two columns to ignore.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
CODES,DESTINATION,RATE,VALIDITY,COMMENT
355,Albania,0.066,22/02/2010,decrease
35538,Albania,0.066,22/02/2010,decrease
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_NO_HEADER_5COL_TWT);
        $r->setOrderName('TWT-51_PREFIX');
        $r->setShortDescription("CSV file, in TWT format, with telephone prefix, description, rate by minute, and two columns to ignore.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
355,Albania,0.066,22/02/2010,decrease
35538,Albania,0.066,22/02/2010,decrease
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_9COL_GAMMA);
        $r->setOrderName('GAMMA-10_PREFIX');
        $r->setShortDescription("CSV file, in GAMMA format.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
ChargeCode,Description,Peak Per Minute,Off Peak Per Minute,Weekend Per Minute,Peak Call Set Up Charge,Off Peak Call Set Up Charge,Weekend Call Set Up Charge,Minimum Charge
AFG,Afghanistan,0.4299,0.4299,0.4299,0,0,0,0.01
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_6COL_GAMMA_ITEM_RENTAL);
        $r->setOrderName('GAMMA-20_PREFIX');
        $r->setShortDescription("CSV file, in GAMMA format for Item Rental.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
type,service,Frequency,Rules,Cost,Applied
Advanced Call Services,Automatic IDA Barring,Rental Per (1 Month),,0.39,Charge per quantity
Advanced IP Services,Fax to Email 0333,Rental Per (1 Month),,5,Charge per quantity
Advanced IP Services,Fax to Email 0844,Rental Per (1 Month),,4,Charge per quantity
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_3COL_PDR);
        $r->setOrderName('70-CALL');
        $r->setShortDescription("CSV file, with header, telophone prefix, description and rate.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
Prefix,Description,Rate
3931,"Mobile prefix",0.04
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_3COL_PDR_IT);
        $r->setOrderName('80-CALL');
        $r->setShortDescription("CSV file, with header, telophone prefix, description and rate, in Italian format.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
Prefix,Description,Rate
3931;"Mobile prefix";0,04
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_DIGITEL_NNG);
        $r->setOrderName('90-CALL');
        $r->setShortDescription("Digitel format for NNG calls.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"Numerazione","Operatore-Servizio","Peak","Off-Peak","Scatto","Durata Minima"
112,"Carabinieri",0,0,0,0
113,"Soccorso Pubblica Emergenza",0,0,0,0
114,"Emergenza Infanzia",0,0,0,0
115,"Vigili del Fuoco",0,0,0,0
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_ECN);
        $r->setOrderName('91-CALL');
        $r->setShortDescription("ECN format, with variable number of off-peak codes");
        $r->setDetailedDescription('The "peak" and other off-peak code names in the header line must match the off-peak codes defined in the holiday table. Something like: <pre>' . htmlentities('
"Description","Communication channel","Operator code as defined in prefix-table","peak","off-peak"
Acme,national,D057,0.50,0.30
        ') . '</pre>');
        $r->save();

        $r = $this->createRateFormat(self::REFERENCE_CSV_WITH_HEADER_4COL_RATING_CODE_COST_ON_CALL);
        $r->setOrderName('RATING-CODE-10');
        $r->setShortDescription("CSV file, with header. Rows are: operator name, rating-code, cost by minute, cost on call.");
        $r->setDetailedDescription('Something like: <pre>' . htmlentities('
"OPERATOR","RATING-CODE","RATE BY MINUTE","COST ON CALL"
"Vodacom","D001",0.1175,0
        ') . '</pre>');
        $r->save();

        return '';
    }
}

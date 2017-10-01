<?php

class Upgrade_2017_07_18 extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function isDBUpgradeJob()
    {
        return true;
        // use true also if it is not correct, otherwise the job is not invoked
    }

    public function process()
    {
        // The next code needs an updated database schema
        $job1 = new InitWithDefaultMySQLStoredProcedures();
        $job1->process();

        // Now the rates are not anymore saved in compressed way.
 
        $c = new Criteria();
        $rates = ArRatePeer::doSelect($c);

        foreach($rates as $rate) {
            $d = $rate->getSourceDataFile();
            if (!is_null($d)) {
                $rate->setSourceDataFile(gzuncompress(base64_decode(stream_get_contents($d))));
            }

            $d = $rate->getBackupSourceDataFile();
            if (!is_null($d)) {
                $rate->setBackupSourceDataFile(gzuncompress(base64_decode(stream_get_contents($d))));
            }

            $rate->save();
        }
        return '';
    }
}

<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Import CSV files from Gamma provider.
 * This is an abstract class. Every Gamma account must be mapped to a distinct job.
 */
abstract class ImportFromGammaProvider extends ImportCSVFilesFromFTPServer
{

    const GAMMA_FILE_TYPE = 'gamma';
    const GAMMA_VERSION = 'v1';
    const GAMMA_ITEM_RENTAL_TYPE = 'gamma-item-rental';
    const GAMMA_ITEM_RENTAL_VERSION = 'v1';

    //
    // Methods to customize in subclasses.
    // Make sure to respect the requirements in method headers.
    //

    /**
     * @return string the name of the account to which the provider expose the files.
     */
    abstract public function getCustomerAccount();

    //
    // Specific Implementation.
    //

    public function getSourceCharacterEncoding() {
        return 'US-ASCII';
    }

    /**
     * @return string the name of the connection params to use, and defined in app.yml under connection settings.
     */
    public function getConnectionName() {
        return 'gamma-' . $this->getCustomerAccount();
    }

    public function getCdrProvider() {
        return $this->getConnectionName();
    }

    /**
     * @return string the remote directory where there are the files to download. Empty for default directory.
     */
    public function getRemoteDirectory() {
       return 'Cdrs';
    }

    public function canAcceptFileName($fileName)
    {
        // Recognize the type of CSV file to import, from its name.
        //
        // Example of filenames to recognize:
        // - FZG_Daily_Calls_XXXXX_07072015_12359525_71_SIP_V3.txt
        // - FZG_Monthly_Calls_XXXXX_31072015_12505688_24_IDA_V3.txt
        // - FZG_Monthly_Calls_XXXXX_31072015_12507291_1721_SIP_V3.txt
        // - XXXXX201506MF20150702112338DISCFF
        //
        // where XXXXX is the name of the account.

        $m = array();

        if (preg_match("/^FZG_(Daily|Monthly)_Calls_" . $this->getCustomerAccount() . "_([0-9]{2})([0-9]{2})([0-9]{4})_[0-9]+_[0-9]+_(IPDCFF|IPDC|IDA|NTSGEOv2|FAX_EMAIL|INBFF|WLRFF|NTS0800|NTS0808|NTS0845|NTS0870|NTS0871|NTSGEO|SIP)_V3\.txt\$/i", $fileName, $m)) {

            $logicalType = self::GAMMA_FILE_TYPE;
            $version = self::GAMMA_VERSION;

            if ($m[1] === 'Daily') {
                $isMonthly = false;
            } else if ($m[1] === "Monthly") {
                $isMonthly = true;
            } else {
                // format not recognized
                return false;
            }

            $fromDay = $m[2];
            $fromMonth = $m[3];
            $fromYear = $m[4];
            $csvType = strtoupper($m[5]);

            // Process first daily files, so monthly files can correct errors.
            if ($isMonthly) {
                $uniquePrefix = 'p2_' . get_ordered_timeprefix_with_unique_id();
            } else {
                $uniquePrefix = 'p1_' . get_ordered_timeprefix_with_unique_id();
            }

            // according original specification:
            // IPDC and SIP -> self::TYPE_SIP
            // IDA -> self::TYPE_PHY
            // NTS0800, NTS0808, NTS -> geographic_nts with PAL normal col
            // NTS0845, NTS0870, NTS0871, NTSGEO, NTSGEOV2 -> geographic_nts without PAL normal col

            $provider = $this->getCdrProvider() . '--calls--' . $csvType;

            if ($csvType === "IPDC" || $csvType == "SIP" || $csvType == "IDA") {
                if ($isMonthly) {
                    return ImportDataFiles::createInputStatusDataFileName($uniquePrefix, $provider, $logicalType, $version, $fromYear, $fromMonth, null);
                } else {
                    return ImportDataFiles::createInputStatusDataFileName($uniquePrefix, $provider, $logicalType, $version, $fromYear, $fromMonth, $fromDay);
                }
            }

            // format not recognized
            return false;

        } else if (preg_match("/^" . $this->getCustomerAccount() . "([0-9]{4})([0-9]{2})MF([0-9]{4})([0-9]{2})[0-9]*(IPDCFF|IPDC|IDA|NTSGEOv2|FAX_EMAIL|INBFF|WLRFF|NTS0800|NTS0808|NTS0845|NTS0870|NTS0871|NTSGEO|DISCFF|ACSFF)(PreviousMonth|PreviousMonthv2|)\.txt\$/i", $fileName, $m)) {

            $logicalType = self::GAMMA_ITEM_RENTAL_TYPE;
            $version = self::GAMMA_ITEM_RENTAL_VERSION;

            $i = 1;
            $fromYear = $m[$i++];
            $fromMonth = $m[$i++];
            $toYear = $m[$i++];
            $toMonth = $m[$i++];
            $csvType = strtoupper($m[$i++]);
            $isReconciliationS = $m[$i++];

            $provider = $this->getCdrProvider() . '--rental--' . $csvType;

            // According specifications:
            // The reconciliation files contain lines were wrong or missing from the daily
            // files.  They contain the lines that were added or changed between the daily
            // and monthly files. They therefore only contain some of the calls that were
            // listed in the monthly file not the whole month's data.  The reconciliation
            // file can be used to check that the monthly file is correct by diffing
            // between the daily and monthly data.  In our case we should just ignore the
            // reconciliation file and overwrite the daily information with that from the
            // monthly file.
            if (! (is_null($isReconciliationS) || strlen($isReconciliationS) == 0)) {
                // ignore reconcilation files
                return true;
            }

            // Check if it is a 1 month status file

            $d1 = strtotime($fromYear . '-' . $fromMonth . '-01');
            $d2 = strtotime($toYear . '-' . $toMonth . '-01');

            if ($d2 !== strtotime('+1 month', $d1)) {
                return false;
            }

            return ImportDataFiles::createInputStatusDataFileName(null, $provider, $logicalType, $version, $fromYear, $fromMonth, null);
        }

        // format not recognized
        return false;
    }
}

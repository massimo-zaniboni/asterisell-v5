<?php

require 'lib/model/om/BaseArBundleState.php';


/**
 * Skeleton subclass for representing a row from the 'ar_bundle_state' table.
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArBundleState extends BaseArBundleState {

    /**
     * Set the content.
     *
     * @param      resource|string|null $d new content
     * @return     Void
     */
    public function setDataFileContentFromPlainText($d)
    {
        if (is_null($d)) {
            $this->setDataFile(null);
        } else {

            if (is_resource($d)) {
                $s = stream_get_contents($d);
            } else {
                $s = $d;
            }

            // use a ascii friendly format for MySQL dump
            $this->setDataFile(base64_encode(gzcompress($s)));
        }
    }

    /**
     * @return string|null
     */
    public function getDataFileContentInPlainText()
    {
        $d = $this->getDataFile();
        if (is_null($d)) {
            return null;
        }

        return gzuncompress(base64_decode(stream_get_contents($d)));
    }

} // ArBundleState

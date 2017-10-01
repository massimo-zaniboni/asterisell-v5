<?php

sfLoader::loadHelpers('I18N');

/**
 * Subclass for representing a row from the 'ar_rate' table.
 *
 * @package lib.model
 */
class ArRate extends BaseArRate
{

    /**
     * Set the content, and save also the backup copy.
     *
     * @param      resource|string|null $d new content
     * @return     Void
     */
    public function setSourceDataFileContentFromPlainText($d)
    {
        $backup = $this->getSourceDataFile();

        if (is_null($d)) {
            $this->setSourceDataFile(null);
        } else {

            if (is_resource($d)) {
                $s = stream_get_contents($d);
            } else {
                $s = $d;
            }

            $this->setSourceDataFile($s);
        }
        $this->setBackupSourceDataFile($backup);
    }

    /**
     * @return string|null
     */
    public function getSourceDataFileContentInPlainText()
    {
        $d = $this->getSourceDataFile();
        if (is_null($d)) {
            return null;
        }
        return stream_get_contents($d);
    }

    /**
     * @return string|null
     */
    public function getBackupSourceDataFileContentInPlainText()
    {
        $d = $this->getBackupSourceDataFile();
        if (is_null($d)) {
            return null;
        }

        return stream_get_contents($d);
    }

    /**
     * Set the content.
     *
     * @param      string|null $d new content
     * @return     Void
     */
    public function setHTMLDescriptionInPlainText($d)
    {
        if (is_null($d)) {
            $this->setHtmlDescription(null);
        } else {
            // use a ascii friendly format for MySQL dump
            $this->setHtmlDescription(base64_encode(gzcompress($d)));
        }
    }

    /**
     * @return string|null
     */
    public function getHTMLDescriptionInPlainText()
    {
        $d = $this->getHtmlDescription();
        if (is_null($d)) {
            return null;
        }

        return gzuncompress(base64_decode(stream_get_contents($d)));
    }


}

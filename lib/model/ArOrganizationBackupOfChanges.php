<?php

require 'lib/model/om/BaseArOrganizationBackupOfChanges.php';


/**
 * Skeleton subclass for representing a row from the 'ar_organization_backup_of_changes' table.
 *
 * 
 *
 * You should add additional methods to this class to meet the
 * application requirements.  This class will only be generated as
 * long as it does not already exist in the output directory.
 *
 * @package    lib.model
 */
class ArOrganizationBackupOfChanges extends BaseArOrganizationBackupOfChanges {

    /**
     * Set the content, and save also the backup copy.
     *
     * @param      resource|string|null $d new content
     * @return     Void
     */
    public function setSqlTablesFromPlainText($d)
    {
        if (is_null($d)) {
            $this->setSqlTables(null);
        } else {

            if (is_resource($d)) {
                $s = stream_get_contents($d);
            } else {
                $s = $d;
            }


            // use a ascii friendly format for MySQL dump
            $this->setSqlTables(base64_encode(gzcompress($s)));
        }
    }

    /**
     * @return string|null
     */
    public function getSqlTablesInPlainText()
    {
        $d = $this->getSqlTables();
        if (is_null($d)) {
            return null;
        }

        return gzuncompress(base64_decode(stream_get_contents($d)));
    }


    /**
     * Set the content, and save also the backup copy.
     *
     * @param      resource|string|null $d new content
     * @return     Void
     */
    public function setYamlExportAtDateFromPlainText($d)
    {
        if (is_null($d)) {
            $this->setYamlExportAtDate(null);
        } else {

            if (is_resource($d)) {
                $s = stream_get_contents($d);
            } else {
                $s = $d;
            }

            // use a ascii friendly format for MySQL dump
            $this->setYamlExportAtDate(base64_encode(gzcompress($s)));
        }
    }

    /**
     * @return string|null
     */
    public function getYamlExportAtDateInPlainText()
    {
        $d = $this->getYamlExportAtDate();
        if (is_null($d)) {
            return null;
        }

        return gzuncompress(base64_decode(stream_get_contents($d)));
    }


} // ArOrganizationBackupOfChanges

<?php
// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Expand extensions like "123*" into "1234", "12345" if they are found in CDRs.
 */
class ExpandExtensions extends FixedJobProcessor
{

    //
    // Customizable Functions
    //

    const INTERNAL_NAME_PREFIX = "expanded_extension_";

    /**
     * @param string $code normal extension code
     * @return string a unique and distinct internal name for the extension
     */
    public function getExpandedExtensionInternalName($code)
    {
        return self::INTERNAL_NAME_PREFIX . $code . "_" . get_ordered_timeprefix_with_unique_id();
    }

    /**
     * @param string $code
     * @return string the name of the extension, as displayed in the call report
     */
    public function getExpandedExtensionName($code) {
        return $code;
    }

    /**
     * @param string $code
     * @return string|null the short name of the extension, as displayed in the call report.
     * null for not using short names, but only normal names.
     */
    public function getExpandedExtensionUserCode($code) {
        return null;
    }

    //
    // Default Interface
    //

    public function process()
    {
        $prof = new JobProfiler('extensions');

        $info = OrganizationUnitInfo::getInstance();

        $conn = Propel::getConnection();
        $conn->beginTransaction();

        try {

            $insertUnit = $conn->prepare('
        INSERT INTO ar_organization_unit
        SET internal_name = ?
        ');

            $insertStructure = $conn->prepare('
        INSERT INTO ar_organization_unit_has_structure
        SET ar_organization_unit_id = LAST_INSERT_ID()
        ,   ar_organization_unit_type_id = ?
        ,   ar_parent_organization_unit_id = ?
        ,   `from` = ?
        ,   `exists` = 1
        ,   ar_rate_category_id = ?
        ,   ar_party_id = NULL
        ,   extension_codes = ?
        ,   extension_name = ?
        ,   extension_user_code = ?
        ');

            $stmt = $conn->prepare('
          SELECT DISTINCT ar_organization_unit_id, extension_code
          FROM ar_expanded_extensions
        ');

            $stmt->execute();
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $prof->incrementProcessedUnits();

                $originalUnitId = $rs[0];
                $expandedCode = $rs[1];

                $data = $info->getDataInfo($originalUnitId, null);
                $priceCategoryId = $data[OrganizationUnitInfo::DATA_STRUCTURE_AR_RATE_CATEGORY_ID];
                $parentId = $data[OrganizationUnitInfo::DATA_STRUCTURE_AR_PARENT_ORGANIZATION_UNIT_ID];
                $from = $data[OrganizationUnitInfo::DATA_STRUCTURE_FROM];
                $unitTypeId = $data[OrganizationUnitInfo::DATA_UNIT_TYPE_ID];

                $insertUnit->execute(array($this->getExpandedExtensionInternalName($expandedCode)));

                $insertStructure->execute(array(
                  $unitTypeId
                , $parentId
                , fromUnixTimestampToMySQLTimestamp($from)
                , $priceCategoryId
                , $expandedCode
                , $this->getExpandedExtensionName($expandedCode)
                , $this->getExpandedExtensionUserCode($expandedCode)));

            }
            $stmt->closeCursor();

            $this->commitTransactionOrSignalProblem($conn);

        } catch (ArProblemException $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);

            $p = ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                  $e
                , get_class($this)
                , "Error in " . get_class($this)
                , "Specific extensions/DIDS found in CDRs can not be inserted. CDRs will be rated and associated to generic extensions/DIDS."
                , null);
            throw($p);
        }

        return $prof->stop();
    }
}

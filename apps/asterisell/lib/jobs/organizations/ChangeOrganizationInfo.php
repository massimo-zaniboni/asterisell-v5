<?php
/* $LICENSE 2014,2017:
 *
 * Copyright (C) 2014,2017 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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
 * Change organization info hierarchy using an YAML content file.
 */
class ChangeOrganizationInfo extends JobProcessor
{
    ////////////
    // PARAMS //
    ////////////

    const ENABLE_DEBUG_MESSAGES = false;

    const GARBAGE_KEY = 'ChangeOrganizationInfo';

    ///////////////////
    // JOB INTERFACE //
    ///////////////////

    public function processEvent(JobData $jobData, $jobId)
    {
        if (!($jobData instanceof ChangeOrganizationInfoEvent)) {
            return null;
        }

        /**
         * @var ChangeOrganizationInfoEvent $jobData
         */
        $this->processYAMLContent($jobData->getYAMLContent());

        return '';
    }

    /////////////
    // SERVICE //
    /////////////

    /**
     * @param string $yamlContent
     * @throws ArProblemException
     */
    public function processYAMLContent($yamlContent)
    {
        try {
            // strip UTF-8 bump encoding, because it causes some problems during parsinng
            $bump_start = "\xEF\xBB\xBF";
            $len = strlen($bump_start);
            if (strcmp(substr($yamlContent, 0, $len), $bump_start) == 0) {
                $spec = sfYaml::load(substr($yamlContent, $len));
            } else {
                $spec = sfYaml::load($yamlContent);
            }

        } catch (Exception $e) {
            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e,
                'parse error - ' . get_class($this),
                'Can not parse YAML content containing Organization Hierarchy information.',
                'Organization Hierarchy will not be updated.',
                'Fix the error on the YAML content, and resubmit it.'));
        }

        if (is_null($spec)) {
            throw ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::VOIP_ACCOUNTS,
                ArProblemResponsible::ADMIN,
                'error in YAML-' . get_class($this) . '-' . md5($yamlContent),
                'Can not parse YAML content containing Organization Hierarchy information.',
                'Organization Hierarchy will not be updated.',
                'Fix the error on the YAML content, and resubmit it.');
        }

        $conn = Propel::getConnection();

        try {
            $conn->beginTransaction();

            $parentId = $this->getRequiredIntOrNullValue($spec, 'parent_id');
            if (array_key_exists('part', $spec)) {
                $partSpec = $this->getRequiredValue($spec, 'part');
                $this->processYAMLPart($partSpec, $parentId, $conn);

            } else if (array_key_exists('parts', $spec)) {
                $partsSpec = $this->getRequiredValue($spec, 'parts');
                foreach ($partsSpec as $listEntry) {
                    $childPart = $this->getRequiredValue($listEntry, 'part');
                    $this->processYAMLPart($childPart, $parentId, $conn);
                }
            }

            $this->commitTransactionOrSignalProblem($conn);

            OrganizationUnitInfo::resetCachedValues();

        } catch (ArProblemException $e) {
            $this->maybeRollbackTransaction($conn);
            throw($e);
        } catch (Exception $e) {
            $this->maybeRollbackTransaction($conn);
            throw(ArProblemException::createFromGenericExceptionWithoutGarbageCollection(
                $e,
                'error in code - ' . get_class($this),
                'Can not process YAML content containing Organization Hieararchy information.',
                'Organization Hierarchy will not be updated.',
                'This is an error in the application code. Contact the assistance.'));
        }
    }

    /**
     * @param array $partSpec
     * @param int|null $parentId the parent of this spec part, null if this is the "virtual" root of the specification
     * @param PDO $conn
     * @throws ArProblemException
     */
    public function processYAMLPart($partSpec, $parentId, PDO $conn)
    {
        // Retrieve basic values that are always expected

        if ($this->getRequiredValue($partSpec, 'id') == 'new_template') {
            return;
        }

        $partId = $this->getRequiredIntOrNewValue($partSpec, 'id');

        $partFromDate = fromUnixTimestampToMySQLTimestamp($this->getRequiredIntOrNullValue($partSpec, 'from_date'));
        $partInternalName = $this->getRequiredStringOrNull($partSpec, 'internal_name');
        $exportCode = $this->getRequiredValue($partSpec, 'export_code');
        if (isEmptyOrNull($exportCode)) {
            $exportCode = null;
        }
        $partType = $this->getRequiredValue($partSpec, 'type');
        $partPriceCategory = $this->getRequiredValue($partSpec, 'price_category');
        $partToDisable = $this->getRequiredBoolValue($partSpec, 'to_disable');

        // Is it an extension or an organization?
        if (array_key_exists('extension', $partSpec)) {
            $extensionSpec = $this->getRequiredValue($partSpec, 'extension');
            $resultStructure_isExtension = true;
        } else {
            $extensionSpec = null;
            $resultStructure_isExtension = false;
        }

        if (array_key_exists('party', $partSpec)) {
            $partySpec = $this->getRequiredValue($partSpec, 'party');
        } else {
            $partySpec = null;
        }

        if (is_null($partySpec) && is_null($extensionSpec)) {
            throw($this->createErrorInYAML($partSpec, "there is no party info, or extension info."));
        }

        if (!is_null($partySpec) && !is_null($extensionSpec)) {
            throw($this->createErrorInYAML($partSpec, "there is both party info and extension info. There must be only one of them."));
        }

        if (is_null($partFromDate)) {
            throw($this->createErrorInYAML($partSpec, "from_date property value \"$partFromDate\" must be a date in \"YYYY-mm-dd hh:mm:ss\" format."));
        }

        // Match price-category

        if ($partPriceCategory == "parent") {
            $partPriceCategoryId = null;
        } else {

            $stmt = $conn->prepare('SELECT id FROM ar_rate_category WHERE internal_name = ?');
            $stmt->execute(array($partPriceCategory));
            $r = null;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $r = $rs[0];
            }
            $stmt->closeCursor();
            $partPriceCategoryId = $r;

            if (is_null($partPriceCategoryId)) {
                throw($this->createErrorInYAML($partSpec, "unknown price category \"$partPriceCategory\""));
            }
        }

        // Match unit type

        // NOTE: the usage of field name is intentional
        $stmt = $conn->prepare('SELECT id FROM ar_organization_unit_type WHERE name = ?');
        $stmt->execute(array($partType));
        $r = null;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }
        $stmt->closeCursor();
        $partTypeId = $r;

        if (is_null($partTypeId)) {
            throw($this->createErrorInYAML($partSpec, "unknown organization type \"$partType\""));
        }

        // Define the new values of structure

        $resultStructure_unitId = $partId;
        $resultStructure_typeId = $partTypeId;
        $resultStructure_parentId = $parentId;
        $resultStructure_from = $partFromDate;
        $resultStructure_exists = !$partToDisable;
        $resultStructure_categoryId = $partPriceCategoryId;
        $resultStructure_partyId = null;

        if ($resultStructure_isExtension) {
            // Extensions must have these fields configured to no NULL value
            $resultStructure_extensionCodes = "";
            $resultStructure_extensionName = "";
            $resultStructure_extensionUserCode = "";
        } else {
            $resultStructure_extensionCodes = null;
            $resultStructure_extensionName = null;
            $resultStructure_extensionUserCode = null;
        }

        // Match party.
        //
        // Use always a single party associated to an organization, also if the database schema give more freedom.
        // Search for nearest PARTY-ID associated to ORGANIZATION-ID

        $stmt = $conn->prepare('SELECT ar_party_id FROM ar_organization_unit_has_structure WHERE ar_organization_unit_id = ? AND ar_party_id IS NOT NULL AND `exists` = 1 ORDER BY `from` DESC LIMIT 1');
        $stmt->execute(array($partId));
        $r = null;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }
        $stmt->closeCursor();
        $originalPartyId = $r;

        // Match Reseller Code

        $resellerId = null;
        if (!is_null($partySpec)) {
            $resellerCode = $this->getRequiredValue($partySpec, "reseller_short_code");
            if (!isEmptyOrNull($resellerCode)) {
                $stmt = $conn->prepare('SELECT id FROM ar_reseller WHERE internal_name = ? ');
                $stmt->execute(array($resellerCode));

                while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                    $resellerId = $rs[0];
                }
                $stmt->closeCursor();

                if (is_null($resellerId)) {
                    throw($this->createErrorInYAML($partySpec, "unknown reseller short code \"$resellerCode\". Add a reseller with this code, or fix the code in the party specification."));
                }
            }
        }


        // Insert or update the value of the party

        if (!is_null($partySpec)) {

            if (is_null($originalPartyId)) {
                $stmt = $conn->prepare('INSERT INTO ar_party(
                    name, compact_name, external_crm_code, vat,
                    is_billable, legal_address, legal_city, legal_zipcode, legal_state_province, legal_country,
                    email, phone, phone2,
                    max_limit_30, migration_field_for_telephone, migration_field_for_adsl,
                    is_active, ar_reseller_id, payment_iban, payment_bic, payment_sepa, payment_info)
                    VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);');
                $stmt->execute(array(
                    $this->getRequiredValue($partySpec, "name"),
                    $this->getRequiredValue($partySpec, "short_name"),
                    $this->getRequiredValue($partySpec, "crm_code"),
                    $this->getRequiredValue($partySpec, "VAT"),

                    $this->getRequiredBoolValue($partySpec, "is_billable"),
                    $this->getRequiredValue($partySpec, "address"),
                    $this->getRequiredValue($partySpec, "city"),
                    $this->getRequiredValue($partySpec, "zip_code"),
                    $this->getRequiredValue($partySpec, "state_province"),
                    $this->getRequiredValue($partySpec, "country"),

                    $this->getRequiredValue($partySpec, "email"),
                    $this->getRequiredValue($partySpec, "telephone1"),
                    $this->getRequiredValue($partySpec, "telephone2"),

                    from_php_decimal_to_db_decimal($this->getRequiredValue($partySpec, "credit_limit")),

                    $this->getRequiredValue($partySpec, 'telephonic_service_migration_field'),
                    $this->getRequiredValue($partySpec, 'internet_service_migration_field'),
                    $this->getRequiredBoolValue($partySpec, "is_active"),
                    $resellerId,
                    $this->getRequiredValue($partySpec, "payment_iban"),
                    $this->getRequiredValue($partySpec, "payment_bic"),
                    $this->getRequiredValue($partySpec, "payment_sepa"),
                    $this->getRequiredValue($partySpec, "payment_info")
                ));

                $originalPartyId = $conn->lastInsertId();
            } else {
                $stmt = $conn->prepare('UPDATE ar_party SET name = ? , compact_name = ? , external_crm_code = ? , vat = ? , is_billable = ? , legal_address = ? , legal_city = ? , legal_zipcode = ? , legal_state_province = ? , legal_country = ? , email = ? , phone = ? , phone2 = ? , max_limit_30 = ? , migration_field_for_telephone = ?, migration_field_for_adsl = ?, is_active = ? , ar_reseller_id = ?, payment_iban = ?, payment_bic = ?, payment_sepa = ?, payment_info = ? WHERE id = ? ;');
                $stmt->execute(array($this->getRequiredValue($partySpec, "name"),
                    $this->getRequiredValue($partySpec, "short_name"),
                    $this->getRequiredValue($partySpec, "crm_code"),
                    $this->getRequiredValue($partySpec, "VAT"),

                    $this->getRequiredBoolValue($partySpec, "is_billable"),
                    $this->getRequiredValue($partySpec, "address"),
                    $this->getRequiredValue($partySpec, "city"),
                    $this->getRequiredValue($partySpec, "zip_code"),
                    $this->getRequiredValue($partySpec, "state_province"),
                    $this->getRequiredValue($partySpec, "country"),

                    $this->getRequiredValue($partySpec, "email"),
                    $this->getRequiredValue($partySpec, "telephone1"),
                    $this->getRequiredValue($partySpec, "telephone2"),

                    from_php_decimal_to_db_decimal($this->getRequiredValue($partySpec, "credit_limit")),
                    $this->getRequiredValue($partySpec, 'telephonic_service_migration_field'),
                    $this->getRequiredValue($partySpec, 'internet_service_migration_field'),

                    $this->getRequiredBoolValue($partySpec, "is_active"),
                    $resellerId,
                    $this->getRequiredValue($partySpec, "payment_iban"),
                    $this->getRequiredValue($partySpec, "payment_bic"),
                    $this->getRequiredValue($partySpec, "payment_sepa"),
                    $this->getRequiredValue($partySpec, "payment_info"),

                    // this is the condition
                    $originalPartyId
                ));

            }

            $resultStructure_partyId = $originalPartyId;

            // Process Party TAGS

            if (array_key_exists('tags', $partySpec)) {
                $tagsSpec = $this->getRequiredValue($partySpec, 'tags');

                // From tag names to tag ids
                $stmt = $conn->prepare('SELECT id FROM ar_tag WHERE internal_name = ?');
                $tagsIds = array();
                foreach ($tagsSpec as $tagName) {
                    $stmt->execute(array($tagName));
                    $tagId = null;
                    while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                        $tagId = $rs[0];
                    }
                    $stmt->closeCursor();
                    if (is_null($tagId)) {
                        throw($this->createErrorInYAML($partySpec, "undefined tag \"" . $tagName . "\""));
                    }
                    $tagsIds[] = $tagId;
                }

                // Delete old info because it will be replaced with new info.
                $stmt = $conn->prepare('DELETE FROM ar_party_has_tag WHERE ar_party_id = ?');
                $stmt->execute(array($originalPartyId));

                // Insert specified tags
                $stmt = $conn->prepare('INSERT INTO ar_party_has_tag(ar_party_id, ar_tag_id) VALUES(?,?)');
                foreach ($tagsIds as $tagId) {
                    $stmt->execute(array($originalPartyId, intval($tagId)));
                }

            } else {
                throw($this->createErrorInYAML($partySpec, "missing tags specification."));
            }
        }

        // Match extension

        if (!is_null($extensionSpec)) {
            $resultStructure_extensionName = $this->getRequiredNotNullString($extensionSpec, 'name');
            $resultStructure_extensionUserCode = $this->getRequiredNotNullString($extensionSpec, 'short_name');
            $resultStructure_extensionCodes = $this->getRequiredNotNullString($extensionSpec, 'codes');
        }

        // Match unit_id

        if (is_null($partId)) {
            $stmt = $conn->prepare('INSERT INTO ar_organization_unit(internal_name, export_code) VALUES(?,?)');
            $stmt->execute(array($partInternalName, $exportCode));
            $originalUnitId = $conn->lastInsertId();
            $partId = $originalUnitId;
            $resultStructure_unitId = $partId;

        } else {
            $stmt = $conn->prepare('SELECT id FROM ar_organization_unit WHERE id = ?');
            $stmt->execute(array($partId));
            $r = null;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                $r = $rs[0];
            }
            $stmt->closeCursor();
            $originalUnitId = $r;

            if (is_null($originalUnitId)) {
                throw($this->createErrorInYAML($partSpec, "unknown id \"$partId\""));
            }

            $stmt = $conn->prepare('UPDATE ar_organization_unit SET internal_name = ?, export_code = ? WHERE id = ?');
            $stmt->execute(array($partInternalName, $exportCode, $originalUnitId));
        }

        // Match structure_id
        //
        // The info is an exact match to fix only if there is already a pair organizationId + fromDate.
        // Otherwise we are adding new info in the history.

        $stmt = $conn->prepare('SELECT id FROM ar_organization_unit_has_structure WHERE ar_organization_unit_id = ? AND `from` = ?');
        $stmt->execute(array($partId, $partFromDate));
        $r = null;
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }
        $stmt->closeCursor();
        $originalStructureId = $r;

        if (is_null($originalStructureId)) {
            // add info to the history

            $stmt = $conn->prepare('INSERT INTO ar_organization_unit_has_structure(
            ar_organization_unit_id,
            ar_organization_unit_type_id,
            ar_parent_organization_unit_id,
            `from`,
            `exists`,
            ar_rate_category_id,
            ar_party_id,
            extension_codes,
            extension_name,
            extension_user_code) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?);');

            $stmt->execute(array(
                $resultStructure_unitId
            , $resultStructure_typeId
            , $resultStructure_parentId
            , $resultStructure_from
            , $resultStructure_exists
            , $resultStructure_categoryId
            , $resultStructure_partyId
            , $resultStructure_extensionCodes
            , $resultStructure_extensionName
            , $resultStructure_extensionUserCode
            ));

            $originalStructureId = $conn->lastInsertId();

        } else {
            // fix the history

            if ($partToDisable) {
                // delete directly the info

                $stmt = $conn->prepare('DELETE FROM ar_organization_unit_has_structure WHERE id = ?');
                $stmt->execute(array($originalStructureId));

            } else {
                $stmt = $conn->prepare('
                  UPDATE ar_organization_unit_has_structure
                  SET ar_organization_unit_id = ? ,
                  ar_organization_unit_type_id = ? ,
                  ar_parent_organization_unit_id = ? ,
                  `from` = ? ,
                  `exists` = ? ,
                  ar_rate_category_id = ? ,
                  ar_party_id = ? ,
                  extension_codes = ? ,
                  extension_name = ? ,
                  extension_user_code = ?
                  WHERE id = ?;');
                $stmt->execute(array(
                    $resultStructure_unitId
                , $resultStructure_typeId
                , $resultStructure_parentId
                , $resultStructure_from
                , $resultStructure_exists
                , $resultStructure_categoryId
                , $resultStructure_partyId
                , $resultStructure_extensionCodes
                , $resultStructure_extensionName
                , $resultStructure_extensionUserCode
                , $originalStructureId
                ));
            }
        }

        // Manage children

        if (array_key_exists('parts', $partSpec)) {
            $partsSpec = $this->getRequiredValue($partSpec, 'parts');
        } else {
            $partsSpec = null;
        }

        if (!is_null($partsSpec)) {
            foreach ($partsSpec as $listEntry) {
                $childPart = $this->getRequiredValue($listEntry, 'part');
                $this->processYAMLPart($childPart, $partId, $conn);
            }
        }
    }

    /**
     * @param array $spec
     * @param string $name
     * @return string|int|bool|array
     * @throws ArProblemException
     */
    protected function getRequiredValue($spec, $name)
    {
        if (array_key_exists($name, $spec)) {
            $v = $spec[$name];
            if (is_string($v)) {
                return trim($v);
            } else {
                return $v;
            }
        } else {
            throw($this->createErrorInYAML($spec, 'there is no entry with name "' . $name . '" in ' . var_export($spec, true)));
        }
    }

    /**
     * @param array $spec
     * @param string $name
     * @return string|int|bool|array
     * @throws ArProblemException
     */
    protected function getRequiredStringOrNull($spec, $name)
    {
        if (array_key_exists($name, $spec)) {
            $v = $spec[$name];
            if (isEmptyOrNull($v)) {
                return null;
            } else if (is_string($v)) {
                return trim($v);
            } else {
                return $v;
            }
        } else {
            throw($this->createErrorInYAML($spec, 'there is no entry with name "' . $name . '" in ' . var_export($spec, true)));
        }
    }

    /**
     * @param array $spec
     * @param string $name
     * @return string|int|bool|array
     * @throws ArProblemException
     */
    protected function getRequiredNotNullString($spec, $name)
    {
        if (array_key_exists($name, $spec)) {
            $v = $spec[$name];
            if (is_null($v)) {
                $v = "";
            }

            if (is_string($v)) {
                return trim($v);
            } else {
                return $v;
            }
        } else {
            throw($this->createErrorInYAML($spec, 'there is no entry with name "' . $name . '" in ' . var_export($spec, true)));
        }
    }

    /**
     * @param array $spec
     * @param string $name
     * @return bool
     * @throws ArProblemException
     */
    protected function getRequiredBoolValue($spec, $name)
    {
        $v = $this->getRequiredValue($spec, $name);
        if (is_bool($v)) {
            return $v;
        } else {
            throw($this->createErrorInYAML($spec, "unknown bool value \"$v\", for property $name, it must be \"true\" or \"false\""));
        }
    }

    /**
     * @param array $spec
     * @param string $name
     * @return int|null
     * @throws ArProblemException
     */
    protected function getRequiredIntOrNullValue($spec, $name)
    {
        $v = $this->getRequiredValue($spec, $name);
        if (is_null($v)) {
            return null;
        }

        if (is_int($v)) {
            return $v;
        } else {
            throw($this->createErrorInYAML($spec, "expected integer or \"null\" value, instead of \"$v\", for property $name"));
        }
    }

    /**
     * @param array $spec
     * @param string $name
     * @return int|null return null if the number is equal to new
     * @throws ArProblemException
     */
    protected function getRequiredIntOrNewValue($spec, $name)
    {
        $v = $this->getRequiredValue($spec, $name);
        if ($v == "new") {
            return null;
        }

        if (is_int($v)) {
            return $v;
        } else {
            throw($this->createErrorInYAML($spec, "expected integer or \"new\" value, instead of \"$v\", for property $name"));
        }
    }


    /**
     * @param array $spec
     * @param string $message
     * @return ArProblemException
     */
    protected function createErrorInYAML($spec, $message)
    {
        if (array_key_exists("id", $spec)) {
            $header = "Error in part with id " . $spec['id'] . ", ";
        } else {
            $header = "Error in YAML content, ";
        }

        return ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_ERROR,
            ArProblemDomain::VOIP_ACCOUNTS,
            ArProblemResponsible::ADMIN,
            'error in YAML-' . get_class($this) . '-' . $header . md5($message),
            $header . $message,
            'Organization Hierarchy will not be updated.',
            'Fix the error on the YAML content, and resubmit it.');
    }

    /**
     * @param int|null $organizationUnitId null for displaying all the organizations
     * @param int|null $fromDate null for retrieving the last date with info
     * @param int|null $parentId
     * @return string|null null if there is no info
     */
    public function getYAMLContent($organizationUnitId, $fromDate, $parentId)
    {
        $info = OrganizationUnitInfo::getInstance();

        $conn = Propel::getConnection();
        $conn->beginTransaction();

        // Initial Help Header

        // Set UTF-8 encoding
        $result = "\xEF\xBB\xBF";

        $result .= "# This file uses the YAML syntax. Do not use TABS, but only SPACES.\n"
            . "# Warning: *Do not modify* the \"parent_id\", and \"id\" values, otherwise organization info will be corrupted.\n"
            . "# You can change the parent of an organization/extension, moving it into another parent list of parts, and changing the indentation level.\n"
            . "# You can clone an organization/extension, copying it, and replacing the id value with \"new\".\n"
            . "# The \"new_template\" parts, are automatically added templates that can be edited for adding new information.\n"
            . "# If you leave unchanged the \"from_date\" value, then you are changing the information about an organization/extension, fixing it. There will be no trace of the previous version of the info.\n"
            . "# If you change \"from_date\", then you are saying that before the specified date, it is still valid the previous configuration, while from the new date, it is valid the new specified configuration. In this way you are preserving the old state of the organization/extensions.\n"
            . "# \"to_disable\" can be set to \"true\", for signaling that an organization/extension is not any more active.\n"
            . "# If you want delete an entire organization hierarchy you must set to \"true\" all the \"to_disable\" fields in the hierarchy, and not only the field of the parent.\n"
            . "# \"to_disable\" set to true, delete info from the history if \"from_date\" is not changed.\n"
            . "# \"to_disable\" set to true, add to the history the info that the organization/extension is not any more active, if \"from_date\" has a new value. Before the specified date the organization/extension is still active, from the date it is not any more active.\n"
            . "# IMPORTANT: write all telephone numbers, and account codes, inside \"...\" (something like \"123\"), otherwise they are parsed as numbers, and silently truncated to some integer value.\n";

        //
        // List of organizations to process.
        //

        $result .= "\nparent_id: ";
        if (is_null($parentId)) {
            $result .= 'null';
        } else {
            $result .= $parentId;
        }
        $result .= "\n";
        $organizationUnitIdsToProcess = array();
        if (is_null($organizationUnitId)) {
            if (is_null($fromDate)) {
                $fromRootDate = time();
            } else {
                $fromRootDate = $fromDate;
            }
            $ids = $info->getRootOrganizationsIds($fromRootDate, $fromRootDate);
            foreach ($ids as $id => $isRoot) {
                if ($isRoot) {
                    $organizationUnitIdsToProcess[] = $id;
                }
            }
            $initialScope = 2;
            $result .= "\nparts:";
            $isRootView = true;
        } else {
            $organizationUnitIdsToProcess[] = $organizationUnitId;
            $initialScope = 0;
            $isRootView = false;
        }

        //
        // Cache some values.
        //

        $types = ArOrganizationUnitTypePeer::doSelect(new Criteria());
        $availableTypes = array();
        foreach ($types as $t) {
            /**
             * @var ArOrganizationUnitType $t
             */
            $availableTypes[$t->getId()] = $t->getName();
        }

        $priceCategories = ArRateCategoryPeer::doSelect(new Criteria());
        $availableRateCategories = array();
        foreach ($priceCategories as $t) {
            /**
             * @var ArRateCategory $t
             */
            $availableRateCategories[$t->getId()] = $t->getInternalName();
        }

        $resellers = ArResellerPeer::doSelect(new Criteria());
        $availableResellers = array();
        foreach ($resellers as $t) {
            /**
             * @var ArReseller $t
             */
            $availableResellers[$t->getId()] = $t->getInternalName();
        }

        //
        // Process the info.
        //

        foreach ($organizationUnitIdsToProcess as $id) {
            if ($isRootView) {
                $result .= "\n  -";
            } else {
                $result .= "\n";
            }
            $result .= $this->produceUnitInfo($id, $fromDate, $initialScope, $info, $availableTypes, $availableRateCategories, $availableResellers, $conn);

        }

        // add demo info
        if ($isRootView) {
            $result .= "\n  -";
            $result .= $this->produceDemoUnitInfo(false, time(), $initialScope, $info, $availableTypes, $availableRateCategories, $conn);
        }

        $result .= "\n\n\n";

        $conn->commit();

        return $result;

    }

    /**
     * @param int $id
     * @param int|null $fromDate
     * @param int $scope 0 to ... for the nested level
     * @param OrganizationUnitInfo $info
     * @param array $cachedUnitTypes
     * @param array $cachedRateCategories
     * @param array $availableResellers
     * @param PDO $conn
     * @return string
     */
    protected function produceUnitInfo($id, $fromDate, $scope, OrganizationUnitInfo $info, $cachedUnitTypes, $cachedRateCategories, $availableResellers, PDO $conn)
    {
        $dataInfo = $info->getDataInfo($id, $fromDate);
        $partyId = $dataInfo[OrganizationUnitInfo::DATA_PARTY_ID];

        // Extract TAGS info
        $tags = '[';
        if (!is_null($partyId)) {
            $stmt = $conn->prepare('
                SELECT internal_name
                FROM ar_party_has_tag
                INNER JOIN ar_tag
                ON ar_tag.id = ar_party_has_tag.ar_tag_id
                WHERE ar_party_id = ?');

            $stmt->execute(array($partyId));
            $isFirstTag = true;
            $r = null;
            while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
                if (!$isFirstTag) {
                    $tags .= ',';
                }
                $isFirstTag = false;

                $tags .= $rs[0];
            }
            $stmt->closeCursor();
        }
        $tags .= ']';

        $indent1 = "\n" . str_repeat('  ', $scope);
        $indent2 = "\n" . str_repeat('  ', $scope + 1);
        $indent3 = "\n" . str_repeat('  ', $scope + 2);

        $r = '';
        if (!is_null($dataInfo) && $dataInfo[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS]) {

            $r = $indent1 . 'part: '
                . $indent2 . "id: " . $id
                . $indent2 . '# ' . $this->convertString($info->getFullNameAtDate($id, $fromDate, false, false))
                . $indent2 . "from_date: " . fromUnixTimestampToMySQLTimestamp($dataInfo[OrganizationUnitInfo::DATA_STRUCTURE_FROM])
                . $indent2 . "internal_name: " . $this->convertString($dataInfo[OrganizationUnitInfo::DATA_UNIT_INTERNAL_NAME])
                . $indent2 . "export_code: " . $this->convertString($dataInfo[OrganizationUnitInfo::DATA_EXPORT_CODE])
                . $indent2 . "type: " . $dataInfo[OrganizationUnitInfo::DATA_UNIT_TYPE_NAME]
                . $indent2 . "price_category: ";

            if (is_null($dataInfo[OrganizationUnitInfo::DATA_STRUCTURE_AR_RATE_CATEGORY_ID])) {
                $r .= "parent";
            } else {
                $r .= $cachedRateCategories[$dataInfo[OrganizationUnitInfo::DATA_STRUCTURE_AR_RATE_CATEGORY_ID]];
            }

            $r .= $indent2 . "to_disable: false ";

            if (!is_null($dataInfo[OrganizationUnitInfo::DATA_PARTY_ID])) {
                $r .= $indent2 . 'party:';

                $party = ArPartyPeer::retrieveByPK($dataInfo[OrganizationUnitInfo::DATA_PARTY_ID]);

                $resellerCode = '';
                if (!is_null($party->getArResellerId())) {
                    $resellerCode = $availableResellers[$party->getArResellerId()];
                }

                $r .= $indent3 . 'name: ' . $this->convertString($party->getName())
                    . $indent3 . 'short_name: ' . $this->convertString($party->getCompactName())
                    . $indent3 . 'is_active: ' . $this->showBool($party->getIsActive())
                    . $indent3 . 'is_billable: ' . $this->showBool($party->getIsBillable())
                    . $indent3 . 'tags: ' . $tags
                    . $indent3 . 'email: ' . $this->convertString($party->getEmail())
                    . $indent3 . 'telephone1: ' . $this->convertString($party->getPhone())
                    . $indent3 . 'telephone2: ' . $this->convertString($party->getPhone2())
                    . $indent3 . 'reseller_short_code: ' . $this->convertString($resellerCode)
                    . $indent3 . 'VAT: ' . $this->convertString($party->getVat())
                    . $indent3 . 'crm_code: ' . $this->convertString($party->getExternalCrmCode())
                    . $indent3 . 'country: ' . $this->convertString($party->getLegalCountry())
                    . $indent3 . 'state_province: ' . $this->convertString($party->getLegalStateProvince())
                    . $indent3 . 'city: ' . $this->convertString($party->getLegalCity())
                    . $indent3 . 'zip_code: ' . $this->convertString($party->getLegalZipcode())
                    . $indent3 . 'address: ' . $this->convertString($party->getLegalAddress())
                    . $indent3 . 'credit_limit: ' . $this->convertString($this->showMonetaryValue($party->getMaxLimit30()))
                    . $indent3 . 'telephonic_service_migration_field: ' . $this->convertString($party->getMigrationFieldForTelephone())
                    . $indent3 . 'internet_service_migration_field: ' . $this->convertString($party->getMigrationFieldForAdsl())
                    . $indent3 . 'payment_iban: ' . $this->convertString($party->getPaymentIban())
                    . $indent3 . 'payment_bic: ' . $this->convertString($party->getPaymentBic())
                    . $indent3 . 'payment_sepa: ' . $this->convertString($party->getPaymentSepa())
                    . $indent3 . 'payment_info: ' . $this->convertString($party->getPaymentInfo());
            }

            if ($dataInfo[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF]) {
                $r .= $indent2 . 'extension:'
                    . $indent3 . 'name: ' . $this->convertString($dataInfo[OrganizationUnitInfo::DATA_EXTENSION_NAME])
                    . $indent3 . 'short_name: ' . $this->convertString($dataInfo[OrganizationUnitInfo::DATA_EXTENSION_USER_CODE])
                    . $indent3 . 'codes: ' . $this->convertString($this->getExtensionCodes($dataInfo[OrganizationUnitInfo::DATA_STRUCTURE_ID], $conn));
            }

            if (!$dataInfo[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF]) {
                $r .= $indent2 . 'parts:';

                $children = $info->getDirectChildrenAtDate($id, $fromDate, true);
                foreach ($children as $childrenId => $isChildren) {
                    $r .= $indent3 . '-';
                    $r .= $this->produceUnitInfo($childrenId, $fromDate, $scope + 3, $info, $cachedUnitTypes, $cachedRateCategories, $availableResellers, $conn);

                }
                // demo info
                $r .= $indent3 . '-';
                $r .= $this->produceDemoUnitInfo(false, time(), $scope + 3, $info, $cachedUnitTypes, $cachedRateCategories, $conn);
                $r .= $indent3 . '-';
                $r .= $this->produceDemoUnitInfo(true, time(), $scope + 3, $info, $cachedUnitTypes, $cachedRateCategories, $conn);
            }
        }

        return $r;
    }

    /**
     * @param bool $forExtension
     * @param int|null $fromDate
     * @param int $scope 0 to ... for the nested level
     * @param OrganizationUnitInfo $info
     * @param array $cachedUnitTypes
     * @param array $cachedRateCategories
     * @param PDO $conn
     * @return string
     */
    protected function produceDemoUnitInfo($forExtension, $fromDate, $scope, OrganizationUnitInfo $info, $cachedUnitTypes, $cachedRateCategories, PDO $conn)
    {

        $isFirst = true;
        $availableTypes = '';
        foreach ($cachedUnitTypes as $id => $name) {
            if (!$isFirst) {
                $availableTypes .= ", ";
            } else {
                $isFirst = false;
            }

            $availableTypes .= $name;
        }

        $availableRates = '';
        foreach ($cachedRateCategories as $id => $name) {
            $availableRates .= ", $name";
        }

        $indent1 = "\n" . str_repeat('  ', $scope);
        $indent2 = "\n" . str_repeat('  ', $scope + 1);
        $indent3 = "\n" . str_repeat('  ', $scope + 2);

        $r = $indent1 . 'part: '
            . $indent2 . "id: new_template"
            . $indent2 . "# use \"new\" for adding new content. Leave unchanged for doing nothing: it is considered only demo information."
            . $indent2 . "from_date: " . fromUnixTimestampToMySQLTimestamp($fromDate)
            . $indent2 . "# specify from when (inclusive) this info become active. The calls before this date, can not be associated to this organization/extension."
            . $indent2 . "internal_name: \"\""
            . $indent2 . "# Usually this field is completed only in case of automatic processing of unit info. Leave blank. "
            . $indent2 . "export_code: \"\""
            . $indent2 . "# Complete for extensions exported to resellers. "
            . $indent2 . "type: \"\""
            . $indent2 . "# one of this values: $availableTypes"
            . $indent2 . "price_category: parent"
            . $indent2 . "# one of this values: parent (for inheriting the price category of the parent) $availableRates";

        $r .= $indent2 . "to_disable: false "
            . $indent2 . "# do not change this value (there)";

        if (!$forExtension) {
            $r .= $indent2 . 'party:'
                . $indent2 . '# see the Party Form in the Web User Interface for the meaning of these fields.';
            $r .= $indent3 . 'name: '
                . $indent3 . 'short_name: '
                . $indent3 . 'is_active: '
                . $indent3 . 'is_billable: '
                . $indent3 . 'tags: []'
                . $indent3 . 'email: '
                . $indent3 . 'telephone1: '
                . $indent3 . 'telephone2: '
                . $indent3 . 'reseller_short_code: '
                . $indent3 . 'VAT: '
                . $indent3 . 'crm_code: '
                . $indent3 . 'country: '
                . $indent3 . 'state_province: '
                . $indent3 . 'city: '
                . $indent3 . 'zip_code: '
                . $indent3 . 'address: '
                . $indent3 . 'credit_limit: '
                . $indent3 . 'telephonic_service_migration_field: '
                . $indent3 . 'internet_service_migration_field: '
                . $indent3 . 'payment_iban: '
                . $indent3 . 'payment_bic: '
                . $indent3 . 'payment_sepa: '
                . $indent3 . 'payment_info: ';

        }

        if ($forExtension) {
            $r .= $indent2 . 'extension:'
                . $indent3 . 'name: ""'
                . $indent3 . '# The full name of the extension to show in the call report.'
                . $indent3 . 'short_name: ""'
                . $indent3 . '# An optional human readable short name. For example in "John Smith (5321)", "John Smith" is the extension name, and "5321" is the extension short name.'
                . $indent3 . 'codes: ""'
                . $indent3 . '# Extension codes to associate to this logical extension/organization. Extension codes can be separated by comma ",", for example "1212,1222,1244". "123X" matches the telephone number "123" followed by one character. "123\X" matches exactly the "123X" telephone number, with the explicit "X" character. "123XX" matches telephone numbers starting with "123" followed by two characters. "123*" match "123" followed by zero or more characters. "123X*" match "123" followed by one or more characters. "123XX3*" matches "123" followed by 2 chars, then "3" and zero or more characters. "123X*35" is an invalid format. "123\X\*35" matches exactly "123X*35". "123" has more priority respect the pattern "12X".';
        } else {

            $r .= $indent2 . 'parts:'
                . $indent2 . '# specify in case a list of "part"';
        }


        return $r;
    }

    protected function showBool($b)
    {
        if ($b) {
            return 'true';
        } else {
            return 'false';
        }
    }

    protected function showMonetaryValue($v)
    {
        if (is_null($v)) {
            return '';
        } else {
            return from_db_decimal_to_smart_php_decimal($v);
        }
    }

    /**
     * @param int $structureId
     * @param PDO $conn
     * @return string
     */
    protected function getExtensionCodes($structureId, $conn)
    {
        $stmt = $conn->prepare('SELECT extension_codes FROM ar_organization_unit_has_structure WHERE id = ?');
        $stmt->execute(array($structureId));

        $r = '';

        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $r = $rs[0];
        }

        $stmt->closeCursor();

        return $r;
    }

    /**
     * @param string|null $str
     * @return string
     */
    protected function convertString($str)
    {
        if (isEmptyOrNull($str)) {
            return '';
        } else {
            return $this->encode_yaml_string($str);
        }
    }

    /**
     * @param string $str
     * @return string
     */
    protected function encode_yaml_string($str)
    {
        $str2 = str_replace('"', '\\' . '"', $str);
        return '"' . $str2 . '"';
    }
}

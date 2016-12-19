<?php

/* $LICENSE 2014, 2015:
 *
 * Copyright (C) 2014, 2015 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

main($argc, $argv);
exit(0);

function fromUnixTimestampToMySQLTimestamp($d)
{
    if (is_null($d)) {
        return null;
    } else {
        return date('Y-m-d H:i:s', $d);
    }
}

function fromMySQLTimestampToUnixTimestamp($d)
{
    if (is_null($d)) {
        return null;
    } else {
        return strtotime($d);
    }
}

/**
 * @param PDO $conn
 * @param string|null $fromYear
 * @param string|null $fromMonth
 */
function exportCDRS(PDO $conn, $fromYear, $fromMonth)
{


    try {
        $stmt = $conn->prepare('SELECT MIN(calldate), MAX(calldate) FROM cdr;');
        $stmt->execute();
        $rs = $stmt->fetch(PDO::FETCH_NUM);
        $minCalldate = fromMySQLTimestampToUnixTimestamp($rs[0]);
        $maxCalldate = fromMySQLTimestampToUnixTimestamp($rs[1]);
        $stmt->closeCursor();

        if (is_null($fromYear)) {
            $currYear = date('Y', $minCalldate);
            $currMonth = date('m', $minCalldate);
        } else {
            $currYear = $fromYear;
            $currMonth = $fromMonth;
        }

        $currDate = strtotime($currYear . '-' . $currMonth . '-01');

        $tmpFileName = '/tmp/exported_cdrs.csv';

        $query = <<<'NOWDOC'
        SELECT calldate,
               IFNULL(src, '\\N'),
               IFNULL(dst, '\\N'),
               IFNULL(duration, '\\N'),
               IFNULL(billsec, '\\N'),
               IFNULL(accountcode, '\\N'),
               IFNULL(destination_type, '\\N'),
               IFNULL(ar_asterisk_account_id, '\\N'),
               IFNULL(income, '\\N'),
               IFNULL(vendor_id, '\\N'),
               IFNULL(cost, '\\N'),
               IFNULL(ar_telephone_prefix_id, '\\N'),
               IFNULL(cached_internal_telephone_number, '\\N'),
               IFNULL(cached_external_telephone_number, '\\N'),
               IFNULL(external_telephone_number_with_applied_portability, '\\N'),
               IFNULL(cached_masked_external_telephone_number, '\\N'),
               IFNULL(source_cost, '\\N'),
               IFNULL(ar_asterisk_account.account_code, '\\N')
NOWDOC;
        $query .= " INTO OUTFILE '$tmpFileName' ";

        $query .= <<<'NOWDOC'
        CHARACTER SET 'utf8'
        FIELDS TERMINATED BY ','
        OPTIONALLY ENCLOSED BY '"'
        ESCAPED BY '"'
        LINES TERMINATED BY '\n'
        FROM cdr INNER JOIN ar_asterisk_account ON cdr.ar_asterisk_account_id = ar_asterisk_account.id
        WHERE calldate >= ? AND calldate < ?
        AND NOT destination_type = 0
NOWDOC;

        $stmt = $conn->prepare($query);
        echo "\n";

        while ($currDate < $maxCalldate) {
            $nextDate = strtotime('+1 month', $currDate);

            $resultFileName = 'exported_cdrs_from_' . date('Y-m-d', $currDate) . '_to_' . date('Y-m-d', $nextDate) . '.manual-importing__import-from-v3__format1';
            echo "Processing $resultFileName   (max calldate to process is " . date('Y-m-d', $maxCalldate) . ")\n";

            @unlink($tmpFileName);

            $conn->beginTransaction();
            $isOk = $stmt->execute(array(fromUnixTimestampToMySQLTimestamp($currDate), fromUnixTimestampToMySQLTimestamp($nextDate)));
            if ($isOk == FALSE) {
                echo "\nError during execution of query:\n $query\n";
                exit(1);
            }
            $stmt->closeCursor();
            $conn->commit();

            @unlink($resultFileName);
            $isOk = @rename($tmpFileName, $resultFileName);
            if ($isOk == FALSE) {
                echo "\nError during moving of $tmpFileName to $resultFileName\n";
                exit(1);
            }

            $currDate = $nextDate;
        }


    } catch (Exception $e) {
        echo "\nError during processing: " . $e->getMessage() . "\n" . $e->getTraceAsString();
        echo "\n";
        exit(1);
    }
}

function indent($scope)
{
    return "\n" . str_repeat('  ', $scope);
}

/**
 * @param array $cache
 * @param int|null $id
 * @param string|null $inheritableCategory
 * @return string
 */
function getRateCategoryName($cache, $id, $inheritableCategory = null)
{
    if (is_null($id)) {
        if (is_null($inheritableCategory)) {
            return 'parent';
        } else {
            return $inheritableCategory;
        }
    } else {
        if (array_key_exists($id, $cache)) {
            $name = $cache[$id];
            return 'old_' . $id . '_' . $name;
        } else {
            return 'old_' . $id;
        }
    }
}

/**
 * Export users on PHP file that can be executed on the destination machine, and recreate the users,
 * performing the lookup with the already inserted organizations.
 *
 * If an user already exists (use login as criteria) do not add it.
 *
 * Display on screen the performed actions.
 * @param PDO $conn
 */
function exportUsers(PDO $conn)
{

    $c = 0;

    $r = <<<'PHP'

// PHP code loading customers.
// Execute inserting in test/section of asterisell.php and call it.
// Then delete the code, after users are inserted.

    $info = OrganizationUnitInfo::getInstance();

PHP;

    $stm = $conn->prepare('SELECT * FROM ar_web_account');
    $stm->execute();
    while (($rs = $stm->fetch(PDO::FETCH_NAMED)) !== false) {
        $c++;
        if (is_null($rs['deactivate_at'])) {
            $isActiveS = 'true';
        } else {
            $isActiveS = 'false';
        }

        $organizationInternalName = null;
        if (!is_null($rs['ar_office_id'])) {
            $organizationInternalName = 'office_' . $rs['ar_office_id'];
        } else if (!is_null($rs['ar_party_id'])) {
            $organizationInternalName = 'customer_' . $rs['ar_party_id'];
        }

        if (!is_null($organizationInternalName)) {
            $loginS = $rs['login'];
            $passwordS = $rs['password'];

            $r .= "\n\$org = ArOrganizationUnitPeer::retrieveByInternalName('$organizationInternalName');";
            $r .= "\nif (is_null(\$org)) { echo \"\\nMissing link with user " . $rs['login'] . "\"; } else {
              \$d = \$info->getDataInfo(\$org->getId(), time());
              \$partyId = \$d[OrganizationUnitInfo::DATA_PARTY_ID];

              \$user = ArUserPeer::retrieveByLogin('$loginS');
              if (is_null(\$user)) {
                \$user = new ArUser();
              }
              \$user->setIsEnabled($isActiveS);
              \$user->setLogin('$loginS');
              \$user->setArOrganizationUnitId(\$org->getId());
              \$user->setClearPassword('$passwordS');
              \$user->setArPartyId(\$partyId);
              \$user->save();

            }";
        }
    }
    $stm->closeCursor();

    $fileName = 'users_result.php';
    file_put_contents($fileName, $r);

    echo "\nExported $c users on file $fileName";
}

/**
 * @param PDO $conn
 * @param bool $insertLeafs false for adding the extension of the leaf in the parent, and the parent became the leaf.
 */
function exportCustomers(PDO $conn, $insertLeafs)
{

    try {

        // Cache rate categories
        $categories = array();
        $stmt = $conn->prepare('SELECT id, name FROM ar_rate_category');
        $stmt->execute();
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $categories[$rs[0]] = $rs[1];
        }
        $stmt->closeCursor();

        // Extract complex parties.
        // A complex party is a party having more than one office.

        $complexPartyId = array();
        $stmt = $conn->prepare('SELECT ar_office.ar_party_id, count(ar_office.id) FROM ar_office GROUP BY ar_office.ar_party_id HAVING count(ar_office.id) > 1');
        $stmt->execute();
        while (($rs = $stmt->fetch(PDO::FETCH_NUM)) !== false) {
            $complexPartyId[$rs[0]] = true;
        }
        $stmt->closeCursor();

        // Scan each party, office and extension

        $stmtParty = $conn->prepare('SELECT * FROM ar_party WHERE customer_or_vendor = \'C\'');
        $stmtOffice = $conn->prepare('SELECT * FROM ar_office WHERE ar_party_id = ?');
        $stmtExtension = $conn->prepare('SELECT * FROM ar_asterisk_account WHERE ar_office_id = ?');

        $scope = 0;
        $h = fopen('extensions.yaml', 'w');
        fwrite($h, indent($scope) . 'parent_id: null');
        fwrite($h, indent($scope) . 'parts:');
        $scope++;
        $stmtParty->execute();
        while (($rsParty = $stmtParty->fetch(PDO::FETCH_NAMED)) !== false) {

            if ($rsParty['is_active']) {
                $isActive = 'true';
            } else {
                $isActive = 'false';
            }

            $priceCategory = getRateCategoryName($categories, $rsParty['ar_rate_category_id']);

            $creditLimit = $rsParty['max_limit_30'];
            if (is_null($creditLimit)) {
                $creditLimit = '';
            } else {
                $creditLimit = trim($creditLimit);
                // remove most left 4 digits, because they are the precision
                $creditLimit = substr($creditLimit, 0, strlen($creditLimit) - 4);
            }

            fwrite($h, indent($scope) . '-');
            $scope++;
            fwrite($h, indent($scope) . 'part:');
            $scope++;
            fwrite($h, indent($scope) . 'id: new');
            fwrite($h, indent($scope) . 'from_date: 2000-01-01 00:00:00');
            fwrite($h, indent($scope) . 'internal_name: customer_' . $rsParty['id']);
            fwrite($h, indent($scope) . 'export_code:');
            fwrite($h, indent($scope) . 'type: Customer');
            fwrite($h, indent($scope) . 'to_disable: false');
            fwrite($h, indent($scope) . 'price_category: ' . $priceCategory);

            fwrite($h, indent($scope) . 'party:');
            $scope++;

            fwrite($h, indent($scope) . 'name: ' . convertString($rsParty['name']));
            fwrite($h, indent($scope) . 'short_name: ');
            fwrite($h, indent($scope) . 'is_active: ' . $isActive);
            fwrite($h, indent($scope) . 'is_billable: ' . 'true');
            fwrite($h, indent($scope) . 'email: ' . convertString($rsParty['email']));
            fwrite($h, indent($scope) . 'telephone1: ' . convertString($rsParty['phone']));
            fwrite($h, indent($scope) . 'telephone2: ' . convertString($rsParty['phone2']));
            fwrite($h, indent($scope) . 'reseller_short_code: ' . convertString($rsParty['reseller_code']));
            fwrite($h, indent($scope) . 'VAT: ' . convertString($rsParty['vat']));
            fwrite($h, indent($scope) . 'crm_code: ' . convertString($rsParty['external_crm_code']));
            fwrite($h, indent($scope) . 'country: ' . convertString($rsParty['legal_country']));
            fwrite($h, indent($scope) . 'state_province: ' . convertString($rsParty['legal_state_province']));
            fwrite($h, indent($scope) . 'city: ' . convertString($rsParty['legal_city']));
            fwrite($h, indent($scope) . 'zip_code: ' . convertString($rsParty['legal_zipcode']));
            fwrite($h, indent($scope) . 'address: ' . convertString($rsParty['legal_address']));
            fwrite($h, indent($scope) . 'credit_limit: ' . convertString($creditLimit));
            fwrite($h, indent($scope) . 'telephonic_service_migration_field: ');
            fwrite($h, indent($scope) . 'internet_service_migration_field: ');
            $scope--;

            $mustGeneratePartsTag = true;
            $stmtOffice->execute(array($rsParty['id']));
            while (($rsOffice = $stmtOffice->fetch(PDO::FETCH_NAMED)) !== false) {

                $id = $rsOffice['id'];

                $priceCategory = getRateCategoryName($categories, $rsOffice['ar_rate_category_id']);
                $inheritableOfficePriceCategory = null;

                if (array_key_exists($rsOffice['ar_party_id'], $complexPartyId)) {
                    // Generate office info only if there is more than one office.

                    $generatedOffice = true;

                    if ($mustGeneratePartsTag) {
                        $mustGeneratePartsTag = false;
                        fwrite($h, indent($scope) . 'parts:');
                        $scope++;
                    }

                    fwrite($h, indent($scope) . '-');
                    $scope++;
                    fwrite($h, indent($scope) . 'part:');
                    $scope++;
                    fwrite($h, indent($scope) . 'id: new');
                    fwrite($h, indent($scope) . 'from_date: 2000-01-01 00:00:00');
                    fwrite($h, indent($scope) . 'internal_name: office_' . $id);
                    fwrite($h, indent($scope) . 'export_code:');
                    fwrite($h, indent($scope) . 'type: Office');
                    fwrite($h, indent($scope) . 'to_disable: false');
                    fwrite($h, indent($scope) . 'price_category: ' . convertString($priceCategory));

                    fwrite($h, indent($scope) . 'party:');
                    $scope++;

                    fwrite($h, indent($scope) . 'name: ' . convertString($rsOffice['name']));
                    fwrite($h, indent($scope) . 'short_name: ');
                    fwrite($h, indent($scope) . 'is_active: true');
                    fwrite($h, indent($scope) . 'is_billable: false');
                    fwrite($h, indent($scope) . 'email: ');
                    fwrite($h, indent($scope) . 'telephone1: ');
                    fwrite($h, indent($scope) . 'telephone2: ');
                    fwrite($h, indent($scope) . 'reseller_short_code: ');
                    fwrite($h, indent($scope) . 'VAT: ');
                    fwrite($h, indent($scope) . 'crm_code: ');
                    fwrite($h, indent($scope) . 'country: ');
                    fwrite($h, indent($scope) . 'state_province: ');
                    fwrite($h, indent($scope) . 'city: ');
                    fwrite($h, indent($scope) . 'zip_code: ');
                    fwrite($h, indent($scope) . 'address: ' . convertString($rsOffice['description']));
                    fwrite($h, indent($scope) . 'credit_limit: ');
                    fwrite($h, indent($scope) . 'telephonic_service_migration_field: ');
                    fwrite($h, indent($scope) . 'internet_service_migration_field: ');
                    $scope--;
                } else {
                    $generatedOffice = false;
                    $inheritableOfficePriceCategory = $priceCategory;
                }

                $mustGeneratePartsTag2 = true;
                $stmtExtension->execute(array($rsOffice['id']));
                while (($rsExtension = $stmtExtension->fetch(PDO::FETCH_NAMED)) !== false) {

                    if ($mustGeneratePartsTag2) {
                        $mustGeneratePartsTag2 = false;
                        fwrite($h, indent($scope) . 'parts:');
                        $scope++;
                    }

                    $id = $rsExtension['id'];
                    $priceCategory = getRateCategoryName($categories, $rsExtension['ar_rate_category_id'], $inheritableOfficePriceCategory);
                    if ($insertLeafs) {
                        $extensionName = $rsExtension['name'];
                    } else {
                        // use an empty name, so the extension will be not expanded in the call report
                        $extensionName = '';
                    }

                    fwrite($h, indent($scope) . '-');
                    $scope++;
                    fwrite($h, indent($scope) . 'part:');
                    $scope++;
                    fwrite($h, indent($scope) . 'id: new');
                    fwrite($h, indent($scope) . 'from_date: 2000-01-01 00:00:00');
                    fwrite($h, indent($scope) . 'internal_name: extension_' . $id);
                    fwrite($h, indent($scope) . 'export_code:');
                    fwrite($h, indent($scope) . 'type: Extension');
                    fwrite($h, indent($scope) . 'to_disable: false');
                    fwrite($h, indent($scope) . 'price_category: ' . convertString($priceCategory));

                    fwrite($h, indent($scope) . 'extension:');
                    $scope++;
                    fwrite($h, indent($scope) . 'name: ' . convertString($extensionName));
                    fwrite($h, indent($scope) . 'short_name: ');
                    fwrite($h, indent($scope) . 'codes: ' . convertString($rsExtension['account_code']));
                    $scope--;
                    $scope--;
                    $scope--;
                }
                $stmtExtension->closeCursor();

                if (!$mustGeneratePartsTag2) {
                    $scope--;
                }

                if ($generatedOffice) {
                    $scope--;
                    $scope--;
                }
            }
            $stmtOffice->closeCursor();

            if (!$mustGeneratePartsTag) {
                $scope--;
            }
            $scope--;
            $scope--;
        }
        $stmtParty->closeCursor();

        fwrite($h, "\n");
        fclose($h);

    } catch (Exception $e) {
        echo "\nError during processing: " . $e->getMessage() . "\n" . $e->getTraceAsString();
        echo "\n";
        exit(1);
    }
}

/**
 * @param string|null $str
 * @return string
 */
function convertString($str)
{
    if (isEmptyOrNull($str)) {
        return '';
    } else {
        $str2 = encode_csv_value($str, ',', '"');
        if (isPrefixOf('"', $str2)) {
            return $str2;
        } else {
            return '"' . $str2 . '"';
        }
    }
}


/**
 * Test if $prefix is prefix of $number in case insensitive mode.
 *
 * @param string $prefix
 * @param string $number
 * @return bool true if $prefix is a prefix of $number, false otherwise.
 */
function isPrefixOf($prefix, $number)
{
    $prefix = trim($prefix);
    $prefixLen = strlen($prefix);

    $numberLen = strlen($number);

    if ($prefixLen > $numberLen) {
        return false;
    }

    if ($prefixLen == 0) {
        return true;
    }

    $numberPrefix = substr($number, 0, $prefixLen);

    if (substr_compare($prefix, $numberPrefix, 0, $prefixLen, TRUE) == 0) {
        return true;
    } else {
        return false;
    }
}


/**
 * Manage correctly quotation.
 * Convert NULL values to '\N'.
 *
 * @param string|null $dataElement
 * @param string $delimeter field separator
 * @param string $enclosure
 * @return string the value to insert in the CSV field, properly escaped/quoted
 */
function encode_csv_value($dataElement, $delimeter = ',', $enclosure = '"')
{

    $useEnclosure = false;
    if (is_null($dataElement)) {
        // MySQL symbol for NULL
        $dataElement = '\\N';
        $useEnclosure = false;
    } else {
        if (strpos($dataElement, $delimeter) !== FALSE) {
            $useEnclosure = true;
        }

        if (strpos($dataElement, $enclosure) !== FALSE) {
            $useEnclosure = true;

            // Replaces a double quote with two double quotes
            $dataElement = str_replace($enclosure, $enclosure . $enclosure, $dataElement);
        }
    }

    if ($useEnclosure) {
        return $enclosure . $dataElement . $enclosure;
    } else {
        return $dataElement;
    }
}


/**
 * @param string $str
 * @return bool
 */
function isEmptyOrNull($str)
{
    if (is_null($str)) {
        return TRUE;
    } else {
        if (strlen($str) == 0) {
            return TRUE;
        } else {
            return FALSE;
        }
    }
}


function main($argc, $argv)
{

    if ($argc < 5) {
        echo <<<'NOWDOC'
Usage:

  php export_data.php <cdrs|extensions|extensions-without-leaf|users> database-name user password

  php export_data.php some-cdrs database-name user password yyyy mm

NOWDOC;

        exit(1);
    }

    $typeOfExport = trim($argv[1]);
    $database = trim($argv[2]);
    $user = trim($argv[3]);
    $password = trim($argv[4]);

    $conn = new PDO("mysql:host=localhost;dbname=$database", $user, $password);
    $conn->exec("set names utf8");

    if ($typeOfExport == "cdrs") {
        exportCDRS($conn, null, null);
    } else if ($typeOfExport == "some-cdrs") {
        $fromYear = trim($argv[5]);
        $fromMonth = trim($argv[6]);
        exportCDRS($conn, $fromYear, $fromMonth);

    } else if ($typeOfExport == "extensions") {
        exportCustomers($conn, true);
    } else if ($typeOfExport == "extensions-without-leaf") {
        exportCustomers($conn, false);
    } else if ($typeOfExport == "users") {
        exportUsers($conn);
    } else {
        echo "\nUnknown option";
        exit(1);
    }
}


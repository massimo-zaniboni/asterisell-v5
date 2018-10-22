<?php

require 'lib/model/om/BaseArVendorDomainPeer.php';


class ArVendorDomainPeer extends BaseArVendorDomainPeer
{

    /**
     * @static
     * @param string $internalName
     * @return ArVendorDomain|null
     */
    public static function retrieveByInternalName($internalName)
    {
        $criteria = new Criteria();
        $criteria->add(ArVendorDomainPeer::INTERNAL_NAME, $internalName);

        return ArVendorDomainPeer::doSelectOne($criteria);
    }

    /**
     * Search first for an exact match, then for the best prefix.
     *
     * Add an error message (not throwing it), in case of deprecated result.
     *
     * @param string $domain the domain to serch.
     * @param int $date date of CDR to associate to Vendor, in Unixtimestamp format
     * @return array|null list(string $foundDomain, int $arVendorId, int $communicationChannelTypeId)
     * @throws ArProblemException
     */
    static public function getBestMatchingVendor($domain, $date)
    {
        $conn = Propel::getConnection();

        $dateC = fromUnixTimestampToMySQLTimestamp($date);

        $base = 'SELECT id, ar_vendor_id, domain, is_deprecated, deprecation_reason, ar_communication_channel_type_id FROM ar_vendor_domain WHERE ar_vendor_domain.from <= ? AND (ar_vendor_domain.to IS NULL OR ar_vendor_domain.to  > ? ) ';
        $order = 'ORDER BY domain DESC';

        // Search for exact domain

        $query = $base . ' AND (is_prefix = 0 AND is_suffix = 0) AND domain = ? ';

        $stmt = $conn->prepare($query);

        $status = $stmt->execute(array($dateC, $dateC, $domain));

        if ($status === false) {

            $problemDuplicationKey = " sql problem - ArVendorDomainPeer";
            $problemDescription = "Problem on class `ArVendorDomainPeer` with query $query";

            $problemEffect = "Some CDRs can not be rated.";
            $problemProposedSolution = "Contact assistance, because this is a bug in the code.";
            $p = ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_ERROR,
                ArProblemDomain::APPLICATION,
                null,
                $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            throw($p);
        }

        $countResult = 0;
        $result = null;
        $vendorId = null;
        while ($rs = $stmt->fetch(PDO::FETCH_ASSOC)) {
            $countResult++;
            $vendorId = $rs['ar_vendor_id'];
            $channelId = $rs['ar_communication_channel_type_id'];
            $result = array($rs['domain'], $vendorId, $channelId);
        }
        $stmt->closeCursor();

        if ($countResult > 1) {
            $p = self::createErrorForRepeatedVendor('duplicated vendor domain ' . $vendorId, $domain, $date);
            throw($p);
        }

        if ($countResult == 1) {
            // if there is an exact match, return it
            return $result;
        } else {
            // Search best prefix

            $query = $base . ' AND (   (is_prefix = 1 AND is_suffix = 0 AND ? LIKE concat(domain, \'%\'))
                                    OR (is_prefix = 0 AND is_suffix = 1 AND ? LIKE concat(\'%\', domain))
                                    OR (is_prefix = 1 AND is_suffix = 1 AND ? LIKE concat(\'%\', domain, \'%\'))
                                   ) '
                           . $order;

            $stmt = $conn->prepare($query);

            $status = $stmt->execute(array($dateC, $dateC, $domain, $domain, $domain));

            if ($status === false) {

                $problemDuplicationKey = " sql problem - ArVendorDomainPeer";
                $problemDescription = "Problem on class `ArVendorDomainPeer` with query $query";

                $problemEffect = "Some CDRs can not be rated.";
                $problemProposedSolution = "Contact assistance, because this is a bug in the code.";
                $p = ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::APPLICATION,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
                throw($p);
            }


            $bestFit = -1;
            $result = null;
            $isDeprecated = false;
            $deprecationReason = null;
            $id = null;
            while ($rs = $stmt->fetch(PDO::FETCH_ASSOC)) {
                $d = $rs['domain'];
                $fit = strlen($d);
                if ($fit > $bestFit) {
                    $isDeprecated = $rs['is_deprecated'];
                    $deprecationReason = $rs['deprecation_reason'];
                    $id = $rs['id'];
                    $result = array($d, $rs['ar_vendor_id'], $rs['ar_communication_channel_type_id']);
                    $bestFit = $fit;
                } else if ($fit === $bestFit) {
                    $p = self::createErrorForRepeatedVendor('duplicated vendor domain ' . $rs['ar_vendor_id'], $domain, $date);
                    throw($p);
             }
            }
            $stmt->closeCursor();

            if ($isDeprecated == 1 || $isDeprecated == true || $isDeprecated == '1') {
                $msg = "The vendor associated to " .  " domain \"$domain\", at date " . fromUnixTimestampToSymfonyStrTimestamp($date) ." is deprecated.";
                if (!isEmptyOrNull($deprecationReason)) {
                    $msg .= 'The reason is: ' . $deprecationReason;
                }

                $problemDuplicationKey = " deprecated vendor - " . $id;
                $problemDescription = $msg;

                $problemEffect = "CDRs are rated normally. This error is signaled only as a warning.";
                $problemProposedSolution = "Configure better the Vendor Domain Rate.";
                ArProblemException::createWithoutGarbageCollection(
                    ArProblemType::TYPE_ERROR,
                    ArProblemDomain::RATES,
                    null,
                    $problemDuplicationKey, $problemDescription, $problemEffect, $problemProposedSolution);
            }

            return $result;
        }
    }

    protected static function createErrorForRepeatedVendor($key, $domain, $date) {
        $repeatedDescr = " domain \"$domain\", at date " . fromUnixTimestampToSymfonyStrTimestamp($date);
        $repeatedDescription = ("There is more than one vendor associated to $repeatedDescr");
        $repeatedEffect = ("Some CDRs can not be rated.");
        $repeatedProposedSolution = ("Configure better the Vendor Domain Rate.");
        $p = ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_ERROR,
            ArProblemDomain::RATES,
            null,
            $key, $repeatedDescription, $repeatedEffect, $repeatedProposedSolution);
        return $p;
    }

} // ArVendorDomainPeer
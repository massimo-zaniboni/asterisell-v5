<?php

/**
 * Subclass for representing a row from the 'cdr' table.
 *
 * It implements also fast static functions, based on PDO associative arrays.
 * Up to date arrays are faster than PHP Objects, and they prevent also memory leaks.
 *
 * @package lib.model
 */
class ArCdr extends BaseArCdr
{

    public function __toString()
    {
        return '<cdr-' . $this->getId() . '>';
    }

    /**
     * @return string a user-readable name identifying the type of Call (incoming/outgoing/internal)
     */
    public function getTypeName()
    {
        return DestinationType::getName($this->getDestinationType());
    }

    /**
     * @return int
     */
    public function getTypeSymbol()
    {
        return DestinationType::getSymbol($this->getDestinationType());
    }

    /**
     * @return string|null null if the ArCdr is consistent, a string desccribing the problem otherwise.
     */
    public function isConsistent()
    {
        if (is_null($this->getCalldate())) {
            return ("CALLDATE field is null");
        }

        return null;
    }

    /**
     * A synonimous of getExternalTelephoneNumberWithAppliedPortability()
     *
     * @return string
     */
    public function getCachedExternalTelephoneNumberWithAppliedPortability()
    {
        return $this->getExternalTelephoneNumberWithAppliedPortability();
    }

    /**
     * @param string $v
     * @return void
     */
    public function setCachedExternalTelephoneNumberWithAppliedPortability($v)
    {
        $this->setExternalTelephoneNumberWithAppliedPortability($v);
    }

    /**
     * @return ArCurrentProblem|null
     */
    public function getArCurrentProblem() {
        return ArCurrentProblemPeer::retrieveByDuplicationKey($this->getArProblemDuplicationKey());
    }

      /**
     * @return string describe the important CDR fields, for debug porpouse
     */
    public function getDebugDescription()
    {
        $debugDetails = $this->getDebugRatingDetails();
        if (is_null($debugDetails)) {
            $debugDetails = 'this debug info is available only when rating in debug mode, with "php asterisell.php debug rerate YY-MM-DD && php asterisell.php debug jobs"';
        }

        return "\nid: " . $this->getId()
            . "\ncalldate: " . self::showMyValue($this->getCalldate())
            . "\ncount_of_calls: " . self::showMyValue($this->getCountOfCalls())
            . "\ndestination_type: " . self::showDestinationType($this->getDestinationType())
            . "\nis_redirect: " . self::showMyBool($this->getIsRedirect())
            . "\nerror_destination_type: " . self::showMyValue($this->getErrorDestinationType())
            . "\nduration: " . self::showMyValue($this->getDuration())
            . "\nbillsec: " . self::showMyValue($this->getBillsec())
            . "\nar_organization_unit_id: " . self::showMyValue($this->getArOrganizationUnitId())
            . "\nbillable_ar_organization_unit_id: " . self::showMyValue($this->getBillableArOrganizationUnitId())
            . "\nbundle organization_unit: " . self::showMyValue($this->getBundleArOrganizationUnitId())
            . "\ncached_parent_id_hierarchy: " . self::showMyValue($this->getCachedParentIdHierarchy())
            . "\nincome: " . self::showMyValue($this->getIncome())
            . "\nar_vendor_id: " . self::showMyValue($this->getArVendorId())
            . "\ncommunication_channel: " . self::showCommunicationChannel($this->getArCommunicationChannelTypeId())
            . "\ncost: " . self::showMyValue($this->getCost())
            . "\nexpected_cost: " . self::showMyValue($this->getExpectedCost())
            . "\ncost_saving: " . self::showMyValue($this->getCostSaving())
            . "\nar_telephone_prefix_id:  " . self::showMyValue($this->getArTelephonePrefixId())
            . "\ncached_external_telephone_number: " . self::showMyValue($this->calcMaskedTelephoneNumber($this->getCachedExternalTelephoneNumber(), false, false))
            . "\ncached_masked_external_telephone_number: " . self::showMyValue($this->getCachedMaskedExternalTelephoneNumber())
            . "\nexternal_telephone_number_with_applied_portability: " . self::showMyValue($this->calcMaskedTelephoneNumber($this->getExternalTelephoneNumberWithAppliedPortability(), false, false))
            . "\napplied cost rate: " . self::showMyValue($this->getDebugCostRate())
            . "\napplied income rate: " . self::showMyValue($this->getDebugIncomeRate())
            . "\nresidual call duration: " . self::showMyValue($this->getDebugResidualCallDuration())
            . "\napplied income rate on residual call duration: " . self::showMyValue($this->getDebugResidualIncomeRate())
            . "\nleft calls in the bundle: " . self::showMyValue($this->getDebugBundleLeftCalls())
            . "\nleft duration in the bundle: " . self::showMyValue($this->getDebugBundleLeftDuration())
            . "\nleft cost in the bundle: " . self::showMyValue($this->getDebugBundleLeftCost())
            . "\nrating details: " . $debugDetails
            ;
    }

    protected static function showDestinationType($v)
    {
        if (is_null($v)) {
            return "<null>";
        } else {
            return $v . '(' . DestinationType::getUntraslatedName($v) . ')';
        }
    }

    public static function showMyValue($v)
    {
        if (is_null($v)) {
            return "<null>";
        } else {
            return $v;
        }
    }

    public static function showMyBool($v)
    {
        if (is_null($v)) {
            return "<null>";
        } else if ($v) {
            return 'true';
        } else {
            return 'false';
        }
    }

    /**
     * @static
     * @param int|null $typeId
     * @return string
     */
    public static function showCommunicationChannel($typeId)
    {
        if (is_null($typeId)) {
            return "<null>";
        } else {
            $channel = ArCommunicationChannelTypePeer::retrieveByPK($typeId);
            if (is_null($channel)) {
                return "<null>";
            } else {
                return $channel->getName();
            }
        }
    }

    /**
     * Return the masked telephone number.
     *
     * @param string $unmasked
     * @param bool $isNeverToMask
     * @param bool $removeCommonPrefix
     * @return string
     */
    public function calcMaskedTelephoneNumber($unmasked, $isNeverToMask = false, $removeCommonPrefix = true)
    {

        $destinationType = $this->getDestinationType();

        // Remove common/default prefix
        //
        $commonPrefix = sfConfig::get('app_not_displayed_telephone_prefix');
        if ($removeCommonPrefix && $commonPrefix != "-") {
            if (strlen($unmasked) > strlen($commonPrefix)) {
                if (substr($unmasked, 0, strlen($commonPrefix)) === $commonPrefix) {
                    $unmasked = substr($unmasked, strlen($commonPrefix));
                }
            }
        }

        // Apply mask
        //
        $mask = sfConfig::get('app_mask_for_external_telephone_number');

        if ($destinationType == DestinationType::internal) {
            return $unmasked;
        }

        if ($isNeverToMask) {
            return $unmasked;
        }

        if (!is_null($unmasked)) {
            $unmasked = trim($unmasked);
            $len = strlen($unmasked);
            if ($len > $mask) {
                return substr($unmasked, 0, $len - $mask) . str_repeat("X", $mask);
            }
        }

        return $unmasked;
    }

}

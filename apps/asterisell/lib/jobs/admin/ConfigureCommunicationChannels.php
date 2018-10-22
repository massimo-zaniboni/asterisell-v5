<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));


/**
 * Define default communication channels.
 */
class ConfigureCommunicationChannels extends AdminJobProcessor
{

    const VOIP_VENDOR_CHANNEL = 'voip-vendor';
    const LOCAL_NETWORK_CHANNEL = 'local-network';
    const SIP_MOBILE_OPERATOR_CHANNEL = 'SIP-mobile-operator';
    const SIP_FIXED_LINE_OPERATOR_CHANNEL = 'SIP-fixed-line-operator';
    const SIP_INTERNAL_MOBILE_CHANNEL = 'SIP-internal-mobile';
    const SIP_FREE_TRUNK_CHANNEL = 'SIP-free-trunk';
    const BACKUP_CHANNEL = 'backup-channel';
    const SERVICE_CDR_CHANNEL = 'system-service-cdr';

    const VENDOR_NONE = 'none';

    const TO_COMPLETE_ACCORDING_VENDOR_DOMAIN_CHANNEL = 'undef';

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        $this->create(self::VOIP_VENDOR_CHANNEL, 'VoIP Vendor', 'Calls routed using the VoIP vendor channel.');
        $this->create(self::LOCAL_NETWORK_CHANNEL, 'Local Network','Calls  routed inside the local network, used only for internal calls.');
        $this->create(self::SIP_MOBILE_OPERATOR_CHANNEL, 'SIP Mobile Operator', 'Calls routed to and from Vendors/Operators, for incoming and outgoing calls, on mobile lines.');
        $this->create(self::SIP_FIXED_LINE_OPERATOR_CHANNEL, 'SIP Fixed Line Operator', 'Calls routed to and from Vendor/Operators, for incoming and outgoing calls, on national fixed lines, or to international operators.');
        $this->create(self::SIP_INTERNAL_MOBILE_CHANNEL, 'SIP Internal Mobile', 'Calls between an internal extension, and a university mobile phone, used for internal calls.');
        $this->create(self::SIP_FREE_TRUNK_CHANNEL, 'ENUM Calls', 'SIP Free Trunk calls, routed on a free SIP channel.');
        $this->create(self::BACKUP_CHANNEL, 'Backup Channel', 'Calls routed to from Vendor/operators, when other channel are not available.');
        $this->create(self::TO_COMPLETE_ACCORDING_VENDOR_DOMAIN_CHANNEL, 'Undef Channel', self::TO_COMPLETE_ACCORDING_VENDOR_DOMAIN_CHANNEL);
        $this->create(self::SERVICE_CDR_CHANNEL, 'Bundle Services', "Pseudo Calls associated to services, or bundle rates with a fixed cost in a timeframe, and so on.");


        $p = new ArParty();
        $p->setName("none");
        $p->save();

        $v = new ArVendor();
        $v->setInternalName(self::VENDOR_NONE);
        $v->setIsInternal(false);
        // on some settings this vendor has associated costs, so set to not internal, otherwise it is not showed on reports
        $v->setArPartyId($p->getId());
        $v->save();

        $c = new ArVendorDomain();
        $c->setInternalName(self::SERVICE_CDR_CHANNEL);
        $c->setFrom($this->getGlobalStartingDate());
        $c->setTo(null);
        $c->setIsPrefix(true);
        $c->setArVendorId($v->getId());
        $c->setArCommunicationChannelTypeId(ArCommunicationChannelTypePeer::retrieveByInternalName(self::SERVICE_CDR_CHANNEL)->getId());
        $c->setDomain(self::SERVICE_CDR_CHANNEL);
        $c->save();

        return '';
    }

    /**
     * @param string $internalName
     * @param string $name
     * @param $description
     * @return void
     */
    protected function create($internalName, $name, $description) {
        $c = new ArCommunicationChannelType();
        $c->setName($name);
        $c->setDescription($description);
        $c->setInternalName($internalName);
        $c->save();
    }

}
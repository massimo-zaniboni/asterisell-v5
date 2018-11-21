<?php

// SPDX-License-Identifier: GPL-3.0-or-later

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Send a test email to the address configured in Params.
 */
class SendTestEmail extends FixedJobProcessor
{

    public function process()
    {
        $p = ArParamsPeer::getDefaultParams();
        $sender = $p->getInvoicingEmailAddress();
        $senderName = $p->getSenderNameOnInvoicingEmails();

        $subject = 'Test email from Asterisell';
        $body = 'This is a test email, sent because it was requested by the admin.';
        $this->sendEmail($sender, $senderName, array(), $subject, $body);
        return 'Email correctly sent. Check if you can receive it.';
    }
}

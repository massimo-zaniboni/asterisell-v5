<?php

  /**************************************************************
   !!!                                                        !!!
   !!! WARNING: This file is automatic generated.             !!!
   !!!                                                        !!!
   !!! In order to modify this file change the content of     !!!
   !!!                                                        !!!
   !!!    /module_template/invoice_template                   !!!
   !!!                                                        !!!
   !!! and execute                                            !!!
   !!!                                                        !!!
   !!!    sh generate_modules.sh                              !!!
   !!!                                                        !!!
   **************************************************************/

use_helper('Markdown', 'OnlineManual');
// Default culture is en_US
//
echo insertHelp('

## email

Email is sent only if the **Send email to customer** button is pressed and the customer has an email associated.

If there were a problem sending an email, then Asterisell signal the problem, and mark the invoice as not sent.

', array(array('invoicing', 'Invoice Configurations'),
         array('invoices', 'Invoice Generation'),
   ));
?>
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

### Fast Invoice Generation / Regeneration

You can generate invoices for every customer with calls in a given time-frame using the **Invoice Creations** module instead of this module.

### Invoice Update

If you change the cost of calls already invoiced then the invoice is not updated. 

You can force a regeneration of an invoice using the **Regenerate Invoice** button wich is displayed in the edit form of an invoice.
', array(array('invoicing', 'Invoice Configurations'),
         array('invoices', 'Invoice Generation'),
   ));
?>
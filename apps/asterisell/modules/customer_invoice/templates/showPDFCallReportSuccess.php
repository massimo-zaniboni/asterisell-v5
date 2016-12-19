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

use_helper('I18N', 'Debug', 'Date', 'Asterisell');

$pdfReport = $ar_invoice->getBinaryPDFCallReport();

// config/view.yml defines the page result type as text/pdf.
//
echo $pdfReport;
?>
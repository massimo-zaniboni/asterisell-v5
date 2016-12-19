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

echo $ar_invoice->getHtmlDetails();
echo "<br/>";

echo "<table border=\"0\"><tr>";

$t = $ar_invoice->getPdfInvoice();
if (!is_null($t)) {
    if (strlen($t) > 0) {
      echo "<td>" . submit_tag(__('Show PDF Invoice'), array('name' => 'show_pdf_report')) . "</td>";
    }
  }

$t = $ar_invoice->getPdfCallReport();
if (!is_null($t)) {
      if (strlen($t)) > 0) {
        echo "<td>" . submit_tag(__('Show PDF Call Report'), array('name' => 'show_pdf_call_report')) . "</td>";;
      }
}

echo "</tr></table>";

?>
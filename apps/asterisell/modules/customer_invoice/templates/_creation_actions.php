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

echo "<table border=\"0\"><tr>";

echo "<td>" . submit_tag(__('Generate Invoice'), array('name' => 'regenerate_invoice')) . "</td>";

echo "<td>" . submit_tag(__('Display Online / Send email to customer'), array('name' => 'send_email_to_customer')) . "</td>";;

echo "</tr></table>";

?>

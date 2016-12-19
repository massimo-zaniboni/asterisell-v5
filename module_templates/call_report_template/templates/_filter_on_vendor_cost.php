<?php

require 'generator_header.php';

echo '<?php';

?>
  /**************************************************************
   !!!                                                        !!!
   !!! WARNING: This file is automatic generated.             !!!
   !!!                                                        !!!
   !!! In order to modify this file change the content of     !!!
   !!!                                                        !!!
   !!!    /module_template/call_report_template               !!!
   !!!                                                        !!!
   !!! and execute                                            !!!
   !!!                                                        !!!
   !!!    sh generate_modules.sh                              !!!     
   !!!                                                        !!!
   **************************************************************/



<?php if ($generateForAdmin) { ?>
    $options = array();
    $d = DestinationType::VENDOR_COST_UNFILTERED;
    $options[$d] = "All Calls";

    $d = DestinationType::VENDOR_COST_DIFFERENT_FROM_EXPECTED;
    $options[$d] = "Unexpected Vendor Cost";

    $defaultChoice = DestinationType::VENDOR_COST_UNFILTERED;
    if (isset($filters['filter_on_vendor_cost'])) {
      $defaultChoice = $filters['filter_on_vendor_cost'];
      if ($defaultChoice != DestinationType::VENDOR_COST_DIFFERENT_FROM_EXPECTED) {
        $defaultChoice = DestinationType::VENDOR_COST_UNFILTERED;
      }
    }
    echo select_tag('filters[filter_on_vendor_cost]', options_for_select($options, $defaultChoice));

<?php } ?>

<?php 
echo '?>' . "\n";
?>

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

use_helper('sfMediaLibrary');

$value = $ar_invoice->getInfoOrAdsImage2();
if (is_null($value)) {
  $value = '';
}
echo input_asset_tag('my_info_image2_file', $value);
?>
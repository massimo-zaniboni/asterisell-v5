<?php

use_helper('Url', 'I18N');

echo link_to(__('download YAML content'), 'backup_of_organizations/downloadyaml?id=' . $ar_organization_backup_of_changes->getId());

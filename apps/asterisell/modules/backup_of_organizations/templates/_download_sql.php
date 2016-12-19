<?php

use_helper('Url', 'I18N');

echo link_to(__('download SQL dump'), 'backup_of_organizations/downloadsql?id=' . $ar_organization_backup_of_changes->getId());

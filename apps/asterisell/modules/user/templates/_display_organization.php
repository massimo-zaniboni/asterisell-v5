<?php

/**
 * @var ArUser $ar_user
 */

if (! is_null($ar_user->getArOrganizationUnitId())) {
    echo htmlspecialchars(OrganizationUnitInfo::getInstance()->getFullNameAtDate($ar_user->getArOrganizationUnitId(),null,false,false,null,false), ENT_QUOTES, 'UTF-8');
}

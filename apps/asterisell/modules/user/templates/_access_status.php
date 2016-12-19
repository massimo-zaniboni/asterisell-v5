<?php

use_helper('Asterisell', 'Url', 'I18N');

/**
 * @var ArUser $ar_user
 */

if ($ar_user->getIsEnabled()) {

    if ($ar_user->isLoginProperlyConfigured()) {
        echo "The user can login, and view call reports online.";
    } else {
        echo "The user is enabled, but he can not login, because login or password are not properly configured.";
    }

    if (is_null($ar_user->getEmailIfEnabled())) {
        echo " He can not receive reports by email, because his email is not configured.";
    } else {
        echo " He can receive reports by email.";
    }
} else {
    echo "The user is disabled: he can not view call reports on-line, and he can not receive reports by emails.";
}

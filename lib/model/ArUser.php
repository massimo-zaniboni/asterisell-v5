<?php

sfLoader::loadHelpers(array('I18N', 'Url', 'Password'));

/**
 * Subclass for representing a row from the 'ar_user' table.
 *
 * @package lib.model
 */
class ArUser extends BaseArUser
{

    /**
     * @param string $password in clear format.
     * @return bool true if the password was set
     */
    public function setClearPassword($password)
    {
        $hash = password_hash($password, PASSWORD_BCRYPT);
        if ($hash === FALSE) {
            // do nothing
            return false;
        } else {
            $this->setPassword($hash);
            return true;
        }
    }

    /**
     * @return bool true if the user is enabled and he has proper login and password
     */
    public function isLoginProperlyConfigured()
    {
        if ($this->getIsEnabled()) {
            if (!isEmptyOrNull($this->getPassword())) {
                if (!isEmptyOrNull($this->getLogin())) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * @return string|null the email associate to the user, if it is enabled, and notifications can be sent
     */
    public function getEmailIfEnabled()
    {
        if ($this->getIsEnabled()) {
            $party = $this->getArParty();
            if (!is_null($party)) {
                $mail = $party->getEmail();
                if (!isEmptyOrNull($mail)) {
                    return $mail;
                }
            }
        }

        return null;
    }

    /**
     * @return bool true if the web account is for an administrator
     */
    public function isAdmin()
    {
        return $this->getIsRootAdmin();
    }

    public function getName()
    {
        if (is_null($this->getArPartyId())) {
            if (is_null($this->getArOrganizationUnitId())) {
                return $this->getLogin();
            } else {
              return OrganizationUnitInfo::getInstance()->getFullNameAtDate($this->getArOrganizationUnitId(), time(), false, false, null, false, false);
            }
        } else {
            $party = ArPartyPeer::retrieveByPK($this->getArPartyId());
            return $party->getName();
        }
    }

    public function __toString()
    {
        return $this->getName();
    }

    /**
     * @return string Html linkable name of a user
     */
    public function getNameAsHtml()
    {
        $userId = $this->getId();
        return '<a href="' . url_for('user/edit?id=' . $userId, TRUE) . '">' . htmlspecialchars($this->getName(), ENT_QUOTES, 'UTF-8') . '</a>';
    }

    /**
     * @return string html representation of the user
     */
    public function getUserPermissionsAsHtml()
    {
        $relations = ArUserHasRolePeer::getRolesOfUser($this->getId());


        $r = '<ul>';
        foreach ($relations as $relation) {
            /**
             * @var ArUserHasRole $relation
             */
            $role = ArRolePeer::retrieveByPK($relation->getArRoleId());
            $r .= '<li>' . htmlspecialchars($role->getName(), ENT_QUOTES, 'UTF-8') . '</li>';
        }

        $r .= '</ul>';

        //

        $relations = ArUserHasPermissionPeer::getPermissionsOfUser($this->getId());

        $r .= '<ul>';
        foreach ($relations as $relation) {
            /**
             * @var ArUserHasPermission $relation
             */
            $permission = ArPermissionPeer::retrieveByPK($relation->getArPermissionId());

            $r .= '<li>' . htmlspecialchars($permission->getName(), ENT_QUOTES, 'UTF-8') . '</li>';
        }

        $r .= '</ul>';

        return $r;
    }

    /**
     * @return string html representation of the user permissions, with roles expanded to permissions
     */
    public function getAllUserPermissionsAsHtml()
    {

        $relations = ArViewAllUserPermissionsPeer::getPermissionsOfUser($this->getId());

        $r = '<ul>';
        foreach ($relations as $relation) {
            /**
             * @var ArViewAllUserPermissions $relation
             */
            $permission = ArPermissionPeer::retrieveByPK($relation->getArPermissionId());

            $r .= '<li>' . htmlspecialchars($permission->getName(), ENT_QUOTES, 'UTF-8') . '</li>';
        }

        $r .= '</ul>';

        return $r;
    }
}

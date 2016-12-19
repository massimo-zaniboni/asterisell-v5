<?php

/* $LICENSE 2009, 2010:
 *
 * Copyright (C) 2009, 2010 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
 *
 * This file is part of Asterisell.
 *
 * Asterisell is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Asterisell is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asterisell. If not, see <http://www.gnu.org/licenses/>.
 * $
 */

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell', 'Password'));
class loginActions extends sfActions
{

    /**
     * Executes index action
     *
     */
    public function executeIndex($request)
    {

    }

    public function executeIndexWithError($request)
    {
        $request->setError('login', __('Password is not correct.'));
        $this->forward('login', 'index');
    }

    protected function forceReloginWithError($request)
    {
        // use this for limiting bad passwords attempt
        sleep(1);
        $this->executeIndexWithError($request);
    }

    public function executeLogin($request)
    {
        $isAdmin = isAdminInstance();

        // Skip in case of maintanance mode
        if ((!$isAdmin) && AsterisellUser::isAppLockedForMaintanance()) {
            return $this->executeLogout($request);
        }

        // Try login

        $c = new Criteria();

        $login = $request->getParameter('login');
        $password = $request->getParameter('password');

        if (isEmptyOrNull($login) || isEmptyOrNull(($password))) {
            // Account configured not correctly.
            $this->getUser()->setAuthenticated(false);
            $this->forceReloginWithError($request);
        }

        if (! (isValidStrForSQLQuery($login) && isValidStrForSQLQuery($password))) {
            // Account configured not correctly.
            $this->getUser()->setAuthenticated(false);
            $this->forceReloginWithError($request);
        }

        $c->add(ArUserPeer::LOGIN, $login);
        $c->add(ArUserPeer::IS_ENABLED, true);
        $c->add(ArUserPeer::IS_ROOT_ADMIN, $isAdmin);
        $webAccounts = ArUserPeer::doSelect($c);

        if (count($webAccounts) > 1) {
            // Account configured not correctly.
            $this->getUser()->logout();
            $this->forceReloginWithError($request);
        } else if (count($webAccounts) == 0) {
            // There is no account with the given name
            $this->getUser()->logout();
            $this->forceReloginWithError($request);
        }

        /**
         * @var ArUser|null $webAccount
         */
        $webAccount = NULL;
        // NOTE: foreach in this case process only one element, because there is only one value
        foreach ($webAccounts as $w) {
            $webAccount = $w;
        }

        $hash = $webAccount->getPassword();
        $passwordIsCorrect = password_verify($password, $hash);
        if (!$passwordIsCorrect) {
            // older version of the application are still using MD5 sum
            $hash = md5($password);
            $passwordIsCorrect = ($hash === $webAccount->getPassword());

            // update the password to the new more secure format
            if ($passwordIsCorrect && isAdminInstance()) {
                $webAccount->setClearPassword($password);
                $webAccount->save();
            }
        }

        if (!$passwordIsCorrect) {
            $this->getUser()->logout();
            $this->forceReloginWithError($request);
        }

        // FROM THIS POINT THE LOGIN IS CORRECT

        /**
         * @var AsterisellUser $user
         */
        $user = $this->getUser();

        $user->login($webAccount);

        $callReportModule = $user->getCallReportModuleName();
        if (! is_null($callReportModule)) {
          return $this->redirect($callReportModule);
        } else {
            $user->logout($request);
            $this->forceReloginWithError($request);
        }
    }

    public function executeLogout($request)
    {
        $this->getUser()->logout();
        return $this->redirect('login/index');
    }

}

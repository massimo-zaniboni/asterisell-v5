<?php

// SPDX-License-Identifier: GPL-3.0-or-later

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

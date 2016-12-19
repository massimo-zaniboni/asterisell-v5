<?php

sfLoader::loadHelpers(array('I18N', 'Debug', 'Asterisell'));

class viewuserActions extends sfActions
{

    public function executeIndex($request)
    {

    }

    public function executeChange($request)
    {
        if ($request->isMethod(sfRequest::POST)) {
            if ($this->getRequestParameter('change')) {

                /**
                 * @var AsterisellUser $user
                 */
                $user = sfContext::getInstance()->getUser();

                $login = $user->getLogin();
                $currentPassword = $request->getParameter('current_password');
                $newPassword1 = $request->getParameter('new_password1');
                $newPassword2 = $request->getParameter('new_password2');

                $arUser = ArUserPeer::retrieveByLogin($login);

                if (is_null($arUser)) {
                    $request->setError('login', __('Unknown user.'));
                    sleep(1);
                    $this->forward('viewuser', 'index');
                }

                if (!(isValidStrForSQLQuery($newPassword1) && isValidStrForSQLQuery($newPassword2))) {
                    $request->setError('login', __('Password contains invalid characters.'));
                    sleep(1);
                    $this->forward('viewuser', 'index');
                }

                if ($newPassword1 !== $newPassword2) {
                    $request->setError('login', __('New password and repeated password are not equal.'));
                    sleep(1);
                    $this->forward('viewuser', 'index');
                }

                if (isEmptyOrNull($newPassword1)) {
                    $request->setError('login', __('New password can not be empty.'));
                    sleep(1);
                    $this->forward('viewuser', 'index');
                }

                if (strlen($newPassword1) < 8) {
                    $request->setError('login', __('New password is too short.'));
                    sleep(1);
                    $this->forward('viewuser', 'index');
                }

                $currentPasswordC = md5($currentPassword);
                $newPassword1C = md5($newPassword1);

                if ($currentPasswordC !== $arUser->getPassword()) {
                    $request->setError('login', __('Invalid Current Password.'));
                    sleep(1);
                    $this->forward('viewuser', 'index');
                }

                $c = new ArUserChangePasswordRequest();
                $c->setArUserId($arUser->getId());
                $c->setAtDate(time());
                $c->setOldPassword($currentPasswordC);
                $c->setNewPassword($newPassword1C);
                $c->setIsProcessed(false);
                $c->save();

                $this->getUser()->setFlash('notice', __('The new password will replace the old password within few minutes.'));
                $this->forward('viewuser', 'index');

            } else {
                /**
                 * @var AsterisellUser $user
                 */
                $user = sfContext::getInstance()->getUser();
                if (!is_null($user)) {
                    $callReportModule = $user->getCallReportModuleName();
                    if (!is_null($callReportModule)) {
                        return $this->redirect($callReportModule);
                    }
                }

                return $this->redirect('login/index');
            }
        }

    }

}

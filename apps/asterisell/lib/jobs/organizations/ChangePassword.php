<?php
// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Change Passwords, if the request is valid.
 */
class ChangePassword extends FixedJobProcessor
{

    public function process()
    {

        // ----------------------------------------------------------
        // Change user passwords using clear_password_to_import field

        $c = new Criteria();
        $c->add(ArUserPeer::CLEAR_PASSWORD_TO_IMPORT, null, Criteria::ISNOTNULL);

        $users = ArUserPeer::doSelect($c);
        foreach ($users as $user) {
            /**
             * @var ArUser $user
             */
            try {
                if ($user->getIsRootAdmin()) {
                    throw($this->signalSecurityCompromise());
                }

                $newPassword = $user->getClearPasswordToImport();
                if (!isEmptyOrNull($newPassword)) {
                  // NOTE: the password will be substitute with the hash
                  $user->setClearPassword($newPassword);
                }

            } catch (ArProblemException $e) {
            } catch (Exception $e) {
                $this->signalSecurityCompromise();
            }

            // in any case process the request
            $user->setClearPasswordToImport(null);
            $user->save();
        }

        // -----------------------------------------------------------------
        // Change user passwords using ar_user_change_password_request table

        $userWithChangedPassword = '';

        $c = new Criteria();
        $c->add(ArUserChangePasswordRequestPeer::IS_PROCESSED, false);
        $c->addAscendingOrderByColumn(ArUserChangePasswordRequestPeer::AT_DATE);

        $requests = ArUserChangePasswordRequestPeer::doSelect($c);
        foreach ($requests as $request) {
            /**
             * @var ArUserChangePasswordRequest $request
             */
            try {
                $user = ArUserPeer::retrieveByPK($request->getArUserId());

                if (is_null($user)) {
                    throw($this->signalSecurityCompromise());
                }

                $userWithChangedPassword .= $user->getLogin() . ', ';

                if ($user->getIsRootAdmin()) {
                    throw($this->signalSecurityCompromise());
                }

                if ($request->getOldPassword() !== $user->getPassword()) {
                    throw($this->signalSecurityCompromise());
                }

                $user->setPassword($request->getNewPassword());
                $user->save();

            } catch (ArProblemException $e) {
            } catch (Exception $e) {
                $this->signalSecurityCompromise();
            }

            // in any case process the request
            $request->setIsProcessed(true);
            $request->save();
        }

        if (!isEmptyOrNull($userWithChangedPassword)) {
            $msg = 'Users changing passwords are: ' . $userWithChangedPassword;

            $problemDuplicationKey = "User changed password - " . time();
            $problemDescription = "The user $userWithChangedPassword changed password.";
            $problemEffect = "None. This is only an informative message.";
            $problemProposedSolution = "Nothing.";
            ArProblemException::createWithoutGarbageCollection(
                ArProblemType::TYPE_INFO,
                ArProblemDomain::VOIP_ACCOUNTS,
                null,
                $problemDuplicationKey,
                $problemDescription,
                $problemEffect,
                $problemProposedSolution);

            return $msg;
        } else {
            return '';
        }

    }

    /**
     * @return ArProblemException
     */
    protected function signalSecurityCompromise()
    {
        $problemDuplicationKey = "User password compromise attempt";
        $problemDescription = "In the table \"ar_user_change_password_request\", and/or \"ar_user.clear_password_to_import\" field there is a bad request of password change. ";
        $problemEffect = "This change of password was blocked, but there is still an unknown security problem in application code, or in the configuration of the instance. ";
        $problemProposedSolution = "Use new passwords for database access, review the security of your server, and contact the application assistance.";
        return ArProblemException::createWithoutGarbageCollection(
            ArProblemType::TYPE_CRITICAL,
            ArProblemDomain::APPLICATION,
            null,
            $problemDuplicationKey,
            $problemDescription,
            $problemEffect,
            $problemProposedSolution
        );
    }
}

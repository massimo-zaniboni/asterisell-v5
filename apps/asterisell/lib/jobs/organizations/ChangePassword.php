<?php
/* $LICENSE 2014:
 *
 * Copyright (C) 2014 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Change Passwords, if the request is valid.
 */
class ChangePassword extends FixedJobProcessor
{

    public function process()
    {
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
                throw($e);
            } catch (Exception $e) {
                throw($this->signalSecurityCompromise());
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
        $problemDescription = "In the table \"ar_user_change_password_request\", there is a bad request of password change. ";
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

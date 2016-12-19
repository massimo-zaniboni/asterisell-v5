<?php

/* $LICENSE 2013:
 *
 * Copyright (C) 2013 Massimo Zaniboni <massimo.zaniboni@asterisell.com>
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


class ConfigureDefaultResponsibleForEachErrorDomainType extends AdminJobProcessor
{

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        // NOTE: by default if there is no setting, the default admin is ArProblemResponsible::ADMIN

        $s = new ArProblemDefaultResponsible();
        $s->setArProblemDomainId(ArProblemDomain::APPLICATION);
        $s->setArProblemResponsibleId(ArProblemResponsible::APPLICATION_ASSISTANCE);
        $s->save();

        $s = new ArProblemDefaultResponsible();
        $s->setArProblemDomainId(ArProblemDomain::CONFIGURATIONS);
        $s->setArProblemResponsibleId(ArProblemResponsible::APPLICATION_ASSISTANCE);
        $s->save();

        return '';
    }

}

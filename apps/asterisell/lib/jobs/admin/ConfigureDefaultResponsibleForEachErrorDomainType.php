<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

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

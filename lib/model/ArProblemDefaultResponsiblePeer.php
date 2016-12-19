<?php

require 'lib/model/om/BaseArProblemDefaultResponsiblePeer.php';


class ArProblemDefaultResponsiblePeer extends BaseArProblemDefaultResponsiblePeer
{

    /**
     * @param int $errorDomainId
     * return int ArProblemDefaultResponsible.id
     */
    static public function getDefaultResponsibleForErrorDomain($errorDomainId)
    {
        static $fromDomainIdToResponsibleId = null;

        if (is_null($fromDomainIdToResponsibleId)) {
            $fromDomainIdToResponsibleId = array();
            $ps = ArProblemDefaultResponsiblePeer::doSelect(new Criteria());
            foreach ($ps as $p) {
                /**
                 * @var ArProblemDefaultResponsible $p
                 */
                $fromDomainIdToResponsibleId[$p->getArProblemDomainId()] = $p->getArProblemResponsibleId();
            }
        }

        if (isset($fromDomainIdToResponsibleId[$errorDomainId])) {
            return $fromDomainIdToResponsibleId[$errorDomainId];
        } else {
            return ArProblemResponsible::ADMIN;
        }

    }
} // ArProblemDefaultResponsiblePeer

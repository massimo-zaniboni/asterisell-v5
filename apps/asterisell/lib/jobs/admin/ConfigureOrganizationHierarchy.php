<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Date', 'Asterisell'));

/**
 * Common class for configuring organization hierarchies.
 */
class ConfigureOrganizationHierarchy extends AdminJobProcessor
{

    const UNSPECIFIED_EXTENSION_INTERNAL_NAME = '__unspecified_extension_internal_code__';

    const EXTENSION_TYPE = "extension";

    public function isCDRTableModified()
    {
        return false;
    }

    public function process()
    {
        return '';
    }

    /**
     * @param int|null $parentId
     * @param string $rootCode the common root code to use as prefix of all created account-codes
     * @param mixed[] $hierarchy
     *
     * $hierarchy is something like
     *
     * > array(
     * >   array('University', 'Reitoria', '', '', array(
     * >     array('Institute', 'Complexo Pedagogico Scientifico Tecnologico', '', '2', array(
     * >          array('Departement', 'Departamento de Eletronica, Telecomunicaziones e Informatica', 'DETI', '2.2', null),
     * >          array('Departement', 'Departamento de Engenharia Civil', 'DECIVIL', '2.3', null),
     * >          array('Departement', 'Departamento de Quimica', 'DQ', '2.4', null),
     * >          array('Departement', 'Departamento de Biologia', 'BIO', '2.5', null)
     * >       )
     * >     )
     *
     * NOTE that account codes as '2.3' as managed like plain strings, and so they are not decomposed automatically
     * in a parent and child relationship, that it is instead represented from the array structure.
     *
     * NOTE: the account-code is put inside the internal-name field of an organization, and so it is managed
     * like an existential property, also if it can change over time.
     */
    public function createOrganizationHierarchy($parentId, $rootCode, $hierarchy)
    {

        $creationDate = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();

        foreach ($hierarchy as $unitInfo) {
            list($typeName, $name, $shortName, $accountCode, $children) = $unitInfo;

            if ($typeName === self::EXTENSION_TYPE) {

                $codes = '"' . $shortName . '"';
                if (!isEmptyOrNull($name)) {
                    $codes .= ',"' . $name . '"';
                }

                $u = new ArOrganizationUnit();
                $u->setInternalName($rootCode . $accountCode);
                $u->save();

                $s = new ArOrganizationUnitHasStructure();
                $s->setArPartyId(null);
                $s->setExtensionName($shortName);
                $s->setExtensionCodes($codes);
                $s->setExtensionUserCode('');
                $s->setArOrganizationUnitId($u->getId());
                $s->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId());
                $s->setArParentOrganizationUnitId($parentId);
                $s->setArRateCategoryId(null);
                $s->setFrom($creationDate);
                $s->setExists(true);
                $s->save();

                echo "\nCreated extension " . $s->getExtensionName();

                // an extension does not have children
                $this->assertCondition(is_null($children) || count($children) === 0, '');

            } else {
                $p = new ArParty();
                $p->setName($name);
                $p->setCompactName($shortName);
                $p->save();

                $t = $this->getOrCreateOrganizationType($typeName, '', null);

                $u = new ArOrganizationUnit();
                $u->setInternalName($rootCode . $accountCode);
                $u->save();

                $s = new ArOrganizationUnitHasStructure();
                $s->setArPartyId($p->getId());
                $s->setArOrganizationUnitId($u->getId());
                $s->setArOrganizationUnitTypeId($t->getId());
                $s->setArParentOrganizationUnitId($parentId);
                $s->setArRateCategoryId($this->getOrCreatePriceCategory('normal')->getId());
                $s->setFrom($creationDate);
                $s->setExists(true);
                $s->save();

                echo "\nCreated organization " . $p->getName();

                if (!is_null($children)) {
                    $this->createOrganizationHierarchy($u->getId(), $rootCode, $children);
                }
            }

        }
    }

    /**
     * @param string $name
     * @param string $shortName
     * @return ArOrganizationUnitType
     */
    protected function getOrCreateOrganizationType($name, $shortName)
    {
        $c = new Criteria();
        $c->add(ArOrganizationUnitTypePeer::NAME, $name);
        $r = ArOrganizationUnitTypePeer::doSelectOne($c);
        if (is_null($r)) {
            $r = new ArOrganizationUnitType();
            $r->setName($name);
            $r->setShortCode($shortName);
            $r->save();
        }

        return $r;
    }

    /**
     * @param string $name
     * @return ArRateCategory
     */
    protected function getOrCreatePriceCategory($name)
    {
        $c = new Criteria();
        $c->add(ArRateCategoryPeer::INTERNAL_NAME, $name);
        $r = ArRateCategoryPeer::doSelectOne($c);
        if (is_null($r)) {
            $r = new ArRateCategory();
            $r->setInternalName($name);
            $r->save();
        }

        return $r;
    }

    /**
     * Content of string is something like
     *
     * > "Type";"SIGLA";"UNIDADE ORGÂNICA";"Account Code";
     * > "unknown";"";Aveiro;"";
     * > "unknown";"ADM";"Administração";"1";
     * > "unknown";"AJ";"Reitoria (Assessoria Jurídica)";1.1;
     * > "unknown";"BIO";"Departamento de Biologia";"1.2";
     * > "extension";"2020";"224040";"1.2";
     * > "unknown";"CCUA";"Reitoria (Conselho Científico)";1.3;
     * > "unknown";"CIFOP";"UINFOC";2;
     * > "unknown";"CPCT";"Complexo Pedagógico Científico Tecnológico";"2.1";
     * > "unknown";"CS";"Secção Autónoma de Ciências da Saúde";"2.2";
     * > "unknown";"CSJP";"Departamento de Ciências Sociais, Políticas e do Território";"2.2.3";
     *
     * The first entry with code "" is always the root.
     *
     * Extension are inserted using the special form:
     *
     * > "extension";"2020";"224040";"1.2";
     *
     * where "224040" is the optional external telephone name associated to the extension.
     *
     * @param string $csvData first line is header, then data
     * @return array in organization format
     */
    public function createOrganizationArrayFromCSVData($csvData)
    {

        $csvLines = explode("\n", $csvData);

        // Something like:
        //
        // $hierarchy = array(
        //   array('University', 'Reitoria', '', array(
        //      array('Institute', 'Complexo Pedagogico Scientifico Tecnologico', '', '', array(
        //          array('Departement', 'Departamento de Eletronica, Telecomunicaziones e Informatica', 'DETI', '2', null),
        //          array('Departement', 'Departamento de Engenharia Civil', 'DECIVIL', '2.1', null),
        //          array('Departement', 'Departamento de Quimica', 'DQ', '2.2', null),
        //          array('Departement', 'Departamento de Biologia', 'BIO', '2.3', null)
        //      )
        //    )
        //
        $hierarchy = array();

        $first = true;
        foreach ($csvLines as $line) {
            if (!$first) {
                $data = str_getcsv($line, ';');

                $type = trim($data[0]);
                $sigla = trim($data[1]);
                $name = trim($data[2]);
                $accountCode = trim($data[3]);

                if (isEmptyOrNull($accountCode)) {
                    $accounts = array('0');
                } else {
                    $accounts = explode('.', '0.' . $accountCode);
                }
                $len = count($accounts);
                $curr = &$hierarchy;
                $i = 0;
                foreach ($accounts as $account) {
                    $i++;
                    if (!array_key_exists($account, $curr)) {
                        $curr[$account] = array('?', '?', '?', array());
                    }

                    if ($i === $len) {
                        $curr[$account][0] = $type;
                        $curr[$account][1] = $name;
                        $curr[$account][2] = $sigla;
                        $curr[$account][3] = $accountCode;
                    } else {
                        $curr = &$curr[$account][4];
                    }
                }
            } else {
                $first = false;
            }
        }

        return $hierarchy;
    }

    /**
     *
     * @param string $csvData
     * @param string $rootCode
     */
    public function createOrganizationHierarchyFromCSVData($csvData, $rootCode = '') {
        $array = $this->createOrganizationArrayFromCSVData($csvData);
        $this->createOrganizationHierarchy(null, $rootCode, $array);
    }

    /**
     * @static
     * @param string $accountCode something like "1.2.3"
     * @return string "1.2", or "" if there is no parent
     */
    static public function getParentAccountCode($accountCode) {
        $r = explode('.', $accountCode);
        if (count($r) > 1) {
            array_pop($r);
            return implode('.', $r);
        } else {
            return '';
        }
    }
}
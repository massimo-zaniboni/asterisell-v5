<?php

// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2009-2019 Massimo Zaniboni <massimo.zaniboni@asterisell.com>

sfLoader::loadHelpers(array('I18N', 'Debug', 'Url', 'Asterisell', 'Form', 'Asset'));

/**
 * Show and edit all the info about a hierarchical organization.
 */
class organization_full_viewActions extends sfActions
{

    /**
     * Initialize common global variables, before processing the request.
     *
     * @param sfWebRequest $request
     */
    protected function initRequest(sfWebRequest $request)
    {
        // Retrieve request parameters
        $id = $request->getParameter('id');
        $this->unitId = $id;
        $this->ArOrganizationUnit = ArOrganizationUnitPeer::retrieveByPk($id);
        if (is_null($this->ArOrganizationUnit)) {
            $msg = "Object ArOrganizationUnit with id=\"$id\" does not exist.";
            $this->getUser()->setFlash('error', $msg);
            $this->forward404($msg);
        }

        // Manage filter date
        $this->isThereFilterDate = true;
        $this->filterDate = $request->getParameter('date');
        if (is_null($this->filterDate)) {
            $this->calcFilterDate = time();
            $this->isThereFilterDate = false;
        } else {
            $this->calcFilterDate = $this->filterDate;
        }

        // Read some common database data
        $this->unitInfo = OrganizationUnitInfo::getInstance()->getDataInfo($id, $this->calcFilterDate);

        // Common URL where jump
        $this->urlParams = $this->asUrlParams($id, $this->filterDate);
        $this->buttonSelectionUrl = $this->getInternalModuleUrl('buttonSelection', $id, $this->filterDate);
        $this->viewActionUrl = $this->getInternalModuleUrl('view', $id, $this->filterDate);
        $this->changeAssociationUrl = $this->getInternalModuleUrl('changeAssociation', $id, $this->filterDate);
        $this->addChildOrganizationUrl = $this->getInternalModuleUrl('addChildOrganization', $id, $this->filterDate);

        $maybeRoleId = $request->getParameter('roleId');
        $this->addRoleUrl = $this->getInternalModuleUrl('addRole', $id, $this->filterDate, null, '&roleId=' . $maybeRoleId);

        $maybePermissionId = $request->getParameter('permissionId');
        $this->addPermissionUrl = $this->getInternalModuleUrl('addPermission', $id, $this->filterDate, null, '&permissionId=' . $maybePermissionId);

        $maybeRelationId = $request->getParameter('relationId');

        $standardExtensionId = ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId();
        if ($this->unitInfo[OrganizationUnitInfo::DATA_UNIT_TYPE_ID] == $standardExtensionId) {
            $this->canAddUser = true;
        } else {
            $this->canAddUser = false;
        }

    }

    /**
     * Intialize the data to display, and then the framework call the template `viewSuccess`.
     *
     * @param sfWebRequest $request
     */
    public function executeView(sfWebRequest $request)
    {
        // Retrieve request parameters

        $id = $request->getParameter('id');
        $this->initRequest($request);

        // Prepare info for the view
        $this->unitInfoShortName = htmlspecialchars(OrganizationUnitInfo::getInstance()->getShortName($this->unitId, $this->filterDate, false, true), ENT_QUOTES, 'UTF-8');
        $this->unitInfoFullName = htmlspecialchars(OrganizationUnitInfo::getInstance()->getFullNameAtDate($this->unitId, $this->filterDate, false, false), ENT_QUOTES, 'UTF-8');
        $this->unitInfoFullNameWithLinks = OrganizationUnitInfo::getInstance()->getFullNameAtDate($this->unitId, $this->filterDate, true, true);
        $this->exportCode = $this->unitInfo[OrganizationUnitInfo::DATA_EXPORT_CODE];

        if ($this->unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS]) {
            $this->unitInfoHtmlHierarchy = $this->getChildrenAsHTMLList($id, $this->filterDate, true);
        }

        $this->currentOrganizationUnitHasStructure = ArOrganizationUnitHasStructurePeer::retrieveByPK($this->unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_ID]);
        $this->htmlListOfDirectChanges = $this->createAListOfChangeAsHtml(OrganizationUnitInfo::getInstance()->getDirectPastModificationDates($this->unitId, $this->filterDate));
        $this->htmlListOfDirectChangesInTheFuture = $this->createAListOfChangeAsHtml(OrganizationUnitInfo::getInstance()->getDirectFutureModificationDates($this->unitId, $this->filterDate));
        $this->lastClassificationDescription = OrganizationUnitInfo::getInstance()->getShortDescription($this->unitId, $this->calcFilterDate, true);

        list($childrenBefore, $currentChildren, $childrenAfter) = OrganizationUnitInfo::getInstance()->classifyDirectChildrenAtDate($this->unitId, $this->calcFilterDate, true);
        $this->htmlListOfDirectChildrenInThePast = $this->createAListOfChangeAsHtml(array_values($childrenBefore));
        $this->htmlListOfDirectChildrenInTheFuture = $this->createAListOfChangeAsHtml(array_values($childrenAfter));

        $this->unitProblems = CheckOrganizationHierarchy::checkAllProblems();

        $this->directPermissions = $this->displayRolesOrPermissionsAsHtml(false, true);
        $this->directRoles = $this->displayRolesOrPermissionsAsHtml(true, false);

        $this->inheritedPermissions = $this->displayInheritedRolesAndPermissionsAsHtml();

        $this->linkToCurrentView = $this->getInternalModuleUrl('view', $this->unitId, null);

        if (count(OrganizationUnitInfo::getInstance()->getAllDirectChildren($this->unitId, $this->calcFilterDate)) === 0) {
            $this->canDeleteOrganization = true;
        } else {
            $this->canDeleteOrganization = false;
        }
    }

    /**
     * Called after the user press some button on the view form.
     * Redirect the request to the correct form.
     *
     * @param sfWebRequest $request
     */
    public
    function executeButtonSelection(sfWebRequest $request)
    {
        $this->initRequest($request);

        if ($request->isMethod(sfRequest::POST)) {

            if ($this->getRequestParameter('association_change')) {
                return $this->redirect($this->getInternalModuleUrl('changeAssociation', $this->unitId, $this->filterDate));

                // TODO vedere se serve ancora...

            } else if ($this->getRequestParameter('classification_fix')) {
                $module = $this->getInternalModuleUrl('changeClassification', $this->unitId, $this->filterDate, ArOrganizationUnitHasStructureForm::FIX_ERROR, '&struct=' . $this->unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_ID]);
                return $this->redirect($module);

            } else if ($this->getRequestParameter('classification_delete')) {
                $module = $this->getInternalModuleUrl('changeClassification', $this->unitId, $this->filterDate, ArOrganizationUnitHasStructureForm::DELETE, '&struct=' . $this->unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_ID]);
                return $this->redirect($module);
            } else if ($this->getRequestParameter('change_from_date')) {
                $module = $this->getInternalModuleUrl('changeClassification', $this->unitId, $this->filterDate, ArOrganizationUnitHasStructureForm::ADD_CHANGE, '&struct=' . $this->unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_ID]);
                return $this->redirect($module);

            } else if ($this->getRequestParameter('disable_from_date')) {
                $module = $this->getInternalModuleUrl('changeClassification', $this->unitId, $this->filterDate, ArOrganizationUnitHasStructureForm::ADD_CHANGE, '&disable=true&struct=' . $this->unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_ID]);
                return $this->redirect($module);


            } else if ($this->getRequestParameter('add_child_organization')) {

                $party = new ArParty();
                $party->setName('New party to configure');
                $party->save();
                $partyId = $party->getId();

                $u = new ArOrganizationUnit();
                $u->save();

                $struct = new ArOrganizationUnitHasStructure();
                $struct->setArPartyId($partyId);
                $struct->setArOrganizationUnitId($u->getId());
                $struct->setArParentOrganizationUnitId($this->unitId);
                $struct->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_GENERIC_ORG)->getId());
                $d = FixedJobProcessor::getOfficialCallDate();
                if (is_null($d)) {
                    $d = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();
                }
                $struct->setFrom($d);
                $struct->save();

                $this->getUser()->setFlash('notice', "Created");
                return $this->redirect($this->getInternalModuleUrl('view', $u->getId(), $this->filterDate));

            } else if ($this->getRequestParameter('add_child_extension')) {

                $u = new ArOrganizationUnit();
                $u->save();

                $struct = new ArOrganizationUnitHasStructure();
                $struct->setExtensionCodes('new-extension');
                $struct->setArOrganizationUnitId($u->getId());
                $struct->setArParentOrganizationUnitId($this->unitId);
                $d = FixedJobProcessor::getOfficialCallDate();
                if (is_null($d)) {
                    $d = FixedJobProcessor::getGlobalStartingDateForCDRProcessinng();
                }
                $struct->setFrom($d);
                $struct->setArOrganizationUnitTypeId(ArOrganizationUnitTypePeer::retrieveByInternalName(ArOrganizationUnitType::ID_FOR_EXTENSION)->getId());
                $struct->save();

                $this->getUser()->setFlash('notice', "Created");
                return $this->redirect($this->getInternalModuleUrl('view', $u->getId(), $this->filterDate));

            } else if ($this->getRequestParameter('show_all_root_organizations')) {
                return $this->redirect('root_organizations/list');
            } else if ($this->getRequestParameter('export_to_yaml') || $this->getRequestParameter('export_all_to_yaml')) {
                $partId = null;
                $parentId = null;
                if ($this->getRequestParameter('export_to_yaml')) {
                    $partId = $this->unitId;
                    $parentId = OrganizationUnitInfo::getInstance()->getParentId($this->unitId, $this->filterDate);
                }

                $job = new ChangeOrganizationInfo();
                $yamlContent = $job->getYAMLContent($partId, $this->filterDate, $parentId);
                if (!isEmptyOrNull($yamlContent)) {
                    $this->setLayout(false);
                    sfConfig::set('sf_web_debug', false);

                    $filename = 'organization_info_';
                    if (is_null($partId)) {
                        $filename .= 'complete';
                    } else {
                        $filename .= $this->unitId;
                    }
                    if (!is_null($this->filterDate)) {
                        $filename .= '_at_date_' . $this->filterDate;
                    }

                    // Adding the file to the Response object
                    $this->getResponse()->clearHttpHeaders();
                    $this->getResponse()->setHttpHeader('Pragma: private', true);
                    $this->getResponse()->setHttpHeader('Content-Disposition', 'attachment; filename="' . $filename . '"');
                    $this->getResponse()->setContentType('text/plain; charset=utf-8');
                    $this->getResponse()->sendHttpHeaders();
                    $this->getResponse()->setContent($yamlContent);

                    return sfView::NONE;
                }

                $this->forwardToItself();
            } else if ($this->getRequestParameter('import_from_yaml')) {
                $fileName = $this->getRequest()->getFilePath('yaml_file');

                if (!is_null($fileName) && (strlen(trim($fileName)) > 0)) {

                    $handle = fopen($fileName, 'r');
                    if ($handle == false) {
                        $this->getUser()->setFlash('error', "Error opening file \"$fileName\"");
                        return $this->redirect($this->getInternalModuleUrl('view', $this->unitId, $this->filterDate));
                    }

                    $content = stream_get_contents($handle);
                    try {
                        $job = new ChangeOrganizationInfo();
                        $job->processYAMLContent($content);
                    } catch (ArProblemException $e) {
                        $this->getUser()->setFlash('error', "Error importing YAML file. " . $e->getLastErrorDescription());
                        return $this->redirect($this->getInternalModuleUrl('view', $this->unitId, $this->filterDate));
                    }
                    fclose($handle);

                    $this->getUser()->setFlash('notice', "YAML file was imported correctly. Previous version of organization hierarchy is saved in backup section.");
                    return $this->redirect($this->getInternalModuleUrl('view', $this->unitId, $this->filterDate));
                }
            } else if ($this->getRequestParameter('save_export_code')) {
                $unit = ArOrganizationUnitPeer::retrieveByPK($this->unitId);
                if (!is_null($unit)) {
                    /*
                     * @var ArOrganizationUnit $unit
                     */
                    $v = $this->getRequestParameter('export_code');
                    if (isEmptyOrNull($v)) {
                        $v = null;
                    }
                    $unit->setExportCode($v);
                    $unit->save();
                    $this->getUser()->setFlash('notice', "Updated export code.");
                }

                return $this->redirect($this->getInternalModuleUrl('view', $this->unitId, $this->filterDate));
            } else {
                return $this->redirect($this->getInternalModuleUrl('view', $this->unitId, $this->filterDate));
            }
        }

        // TODO  manage the SUCCESS that is an error because there is no action to take
    }

    public
    function executeDelete(sfWebRequest $request)
    {
        $this->initRequest($request);

        $parentId = OrganizationUnitInfo::getInstance()->getParentId($this->unitId, $this->filterDate);

        $conn = Propel::getConnection();
        $conn->beginTransaction();

        // Delete all associated structure info
        $query = "DELETE FROM " . ArOrganizationUnitHasStructurePeer::TABLE_NAME . " WHERE " . ArOrganizationUnitHasStructurePeer::AR_ORGANIZATION_UNIT_ID . " = ? ";
        $stmt = $conn->prepare($query);
        $stmt->execute(array($this->unitId));

        // Delete organizations
        $query = "DELETE FROM " . ArOrganizationUnitPeer::TABLE_NAME . " WHERE " . ArOrganizationUnitPeer::ID . " = ? ";
        $stmt = $conn->prepare($query);
        $stmt->execute(array($this->unitId));

        $conn->commit();

        // Go to parent because child does not existn any more
        return $this->redirect($this->getInternalModuleUrl('view', $parentId, $this->filterDate));
    }

    /**
     * Manage the change of a structure. See `ArOrganizationUnitHasStructureForm` for the details of the form.
     *
     * @param sfWebRequest $request
     */
    public
    function executeChangeClassification(sfWebRequest $request)
    {
        $this->initRequest($request);

        if ($request->isMethod('get')) {
            $struct = NULL;
            $typeOfChange = $request->getParameter('type_of_change');
            $this->structId = $request->getParameter('struct');
            $this->forward404Unless($this->structId, 'Object ArOrganizationUnitHasStructure does not exist');
            $this->changeClassificationUrl = $this->getInternalModuleUrl('changeClassification', $this->unitId, $this->filterDate, null, '&struct=' . $this->structId);
            $struct = ArOrganizationUnitHasStructurePeer::retrieveByPK($this->structId);
            $this->forward404Unless($struct, sprintf('Object ArOrganizationUnitHasStructure does not exist (%s).', $this->structId));

            $form = new ArOrganizationUnitHasStructureForm($struct);
            $form->setDefault(ArOrganizationUnitHasStructureForm::TYPE_OF_CHANGE, $typeOfChange);
            if ($request->getParameter('disable') == 'true') {
                $form->setDefault('exists', false);
            }
            $this->form = $form;
            // NOTE: execute template `changeClassificationSucces

        } else if ($request->isMethod('post')) {
            if ($this->getRequestParameter('save')) {
                $struct = NULL;
                $this->structId = $request->getParameter('struct');
                $this->forward404Unless($this->structId, 'Object ArOrganizationUnitHasStructure does not exist');
                $struct = ArOrganizationUnitHasStructurePeer::retrieveByPK($this->structId);
                $this->forward404Unless($struct, sprintf('Object ArOrganizationUnitHasStructure does not exist (%s).', $this->structId));

                $this->form = new ArOrganizationUnitHasStructureForm($struct);
                $this->form->bind($request->getParameter('ar_organization_unit_has_structure'));

                if ($this->form->isValid()) {
                    $processMode = $this->form->getValue(ArOrganizationUnitHasStructureForm::TYPE_OF_CHANGE);
                    if ($processMode == ArOrganizationUnitHasStructureForm::ADD_CHANGE) {
                        // save on a new object
                        $this->form = new ArOrganizationUnitHasStructureForm(new ArOrganizationUnitHasStructure());
                        $r = $request->getParameter('ar_organization_unit_has_structure');
                        $r['id'] = null;
                        $this->form->bind($r);
                        if ($this->form->isValid()) {
                            $this->form->save();
                            $this->getUser()->setFlash('notice', "Added");
                            return $this->redirect($this->viewActionUrl);
                        } else {
                            $this->getUser()->setFlash('error', "Error during saving of data. " . $this->describeFormErrors($this->form));
                            return $this->redirect($this->viewActionUrl);
                        }
                    } else if ($processMode == ArOrganizationUnitHasStructureForm::FIX_ERROR) {
                        // save on current object
                        $this->form->save();
                        $this->getUser()->setFlash('notice', "Saved");
                        return $this->redirect($this->viewActionUrl);

                    } else if ($processMode == ArOrganizationUnitHasStructureForm::DELETE) {

                        $structId = $this->structId;
                        $parentId = $struct->getArParentOrganizationUnitId();
                        $info = OrganizationUnitInfo::getInstance();

                        $returnUrl = $this->viewActionUrl;

                        try {
                            $alsoOrganization = $info->deletePhysicallyUnitStructure($structId);
                            if ($alsoOrganization) {
                                if (is_null($parentId)) {
                                    // the organization was deleted, there is no parent, so jump to the list of root organizations
                                    $returnUrl = url_for('root_organizations/list');
                                } else {
                                    // the organization was deleted, so jump to the parent
                                    $returnUrl = $this->getInternalModuleUrl('view', $parentId, $this->filterDate);
                                }
                            }
                            $this->getUser()->setFlash('notice', "Deleted");
                        } catch (Exception $e) {
                            $this->getUser()->setFlash('error', "This record can not be deleted. " . $e->getMessage());
                        }
                        return $this->redirect($returnUrl);
                    }
                } else {
                    $this->getUser()->setFlash('error', "Error during saving of data. " . $this->describeFormErrors($this->form));
                    return $this->redirect($this->viewActionUrl);
                }

            } else if ($this->getRequestParameter('cancel')) {
                // return without saving
                return $this->redirect($this->viewActionUrl);
            }
        } else {
            $this->getUser()->setFlash('error', "Error during saving of data. (12)");
            return $this->redirect($this->viewActionUrl);
        }
    }

    /**
     * @param BaseFormPropel|null $form
     * @return string
     */
    protected
    function describeFormErrors($form)
    {
        $r = '';
        if (!is_null($form)) {
            $s = $form->getErrorSchema();
            if (!is_null($s)) {
                foreach ($s->getErrors() as $err) {
                    /**
                     * @var sfValidatorError| null $err
                     */
                    if (!is_null($err)) {
                        $r .= __($err->getMessageFormat(), $err->getArguments()) . "\n";
                    }
                }
            }
        }
        return $r;
    }

////////////////
// Manage URL //
////////////////

    /**
     * @param int $id
     * @param int $date
     * @return string
     */
    protected
    function asUrlParams($id, $date)
    {
        $r = '?id=' . $id;
        if (!is_null($date)) {
            $r .= '&date=' . $date;
        }
        return $r;
    }

    /**
     * @param string $module
     * @param int $id
     * @param int $date
     * @param int|null $typeOfChange something like ArOrganizationUnitHasStructureForm::ADD_CHANGE identifying the type of change
     * @param string $other
     * @return string
     */
    protected
    function getInternalModuleUrl($module, $id, $date, $typeOfChange = null, $other = "")
    {
        if (is_null($typeOfChange)) {
            $typeOfChangeParam = '';
        } else {
            $typeOfChangeParam = '&type_of_change=' . $typeOfChange;
        }
        return url_for('organization_full_view/' . $module . $this->asUrlParams($id, $date) . $typeOfChangeParam . $other);
    }

    protected
    function forwardToItself()
    {
        $this->forward('organization_full_view', $this->asUrlParams($this->unitId, $this->filterDate));
    }

    /**
     * @param int $id
     * @param int $date
     * @return string
     */
    protected
    function getFormUrl($id, $date)
    {
        return $this->getInternalModuleUrl('follow', $id, $date);
    }

    /////////////////////////////////////////
    // Display Users Roles and Permissions //
    /////////////////////////////////////////

    /**
     * Generic function that displays roles or permissions.
     *
     * @return string Html representation of user permissions and roles
     */
    protected
    function displayRolesOrPermissionsAsHtml($showRoles, $showPermissions)
    {
        // Initial settings
        $typeName = '';
        if ($showPermissions) {
            $typeName = 'Permission';
        } else if ($showRoles) {
            $typeName = 'Role';
        }

        // Init with an empty array for each group key (role or permission)
        $groups = array();
        if ($showRoles) {
            $all = ArRolePeer::getAll();
            foreach ($all as $a) {
                $groups[$a->getId()] = array();
            }
        } else if ($showPermissions) {
            $all = ArPermissionPeer::getAll();
            foreach ($all as $a) {
                $groups[$a->getId()] = array();
            }
        } else {
            assertCondition(false, "Unexpected no showRoles and no showPermissions.");
        }

        // Insert descriptions lines in $groups array.
        // Each line is a user role or permission
        if ($showPermissions) {
            $relations = OrganizationUnitInfo::getInstance()->getDirectUsersWithAllPermissions($this->unitId);
        } else if ($showRoles) {
            $relations = OrganizationUnitInfo::getInstance()->getDirectUsersWithRoles($this->unitId);
        } else {
            // TODO signal error
        }

        foreach ($relations as $userId => $capabilities) {
            foreach ($capabilities as $capabilityId) {
                $user = ArUserPeer::retrieveByPK($userId);
                $line = __('user') . ' ' . $user->getNameAsHtml();
                array_push($groups[$capabilityId], $line);
            }
        }

        // Generate HTML info
        $r = '<ul class="permissions">';
        foreach ($groups as $groupKeyId => $descriptions) {
            sort($descriptions);
            $groupKeyName = '';
            $groupDescription = '';
            if ($showPermissions) {
                $p = ArPermissionPeer::retrieveByPK($groupKeyId);
                $groupKeyName = $p->getName();
                $groupDescription = ArPermissionPeer::retrieveByPK($groupKeyId)->getDescription();
            } else if ($showRoles) {
                $p = ArRolePeer::retrieveByPK($groupKeyId);
                $groupKeyName = $p->getName();
                $groupDescription = $p->getDescription();
            }

            if (count($descriptions) > 0) {
                $r .= '<li><img align="absmiddle" alt="' . htmlspecialchars($groupDescription, ENT_QUOTES, 'UTF-8') . '" title="' . htmlspecialchars($groupDescription, ENT_QUOTES, 'UTF-8') . '" src="' . image_path("sf/sf_admin/images/help.png") . '"/>' . htmlspecialchars($groupKeyName, ENT_QUOTES, 'UTF-8') . '<ul>';
                foreach ($descriptions as $descr) {
                    $r .= '<li>' . $descr . '</li>';
                }

                $r .= '</li>';

                $r .= '</ul></li>';
            }
        }
        $r .= '</ul>';

        return $r;
    }

    /**
     * Display inherited roles and permissions.
     *
     * @return string
     */
    protected
    function displayInheritedRolesAndPermissionsAsHtml()
    {
        return $this->displayInheritedRolesOrPermissionsAsHtml(true) . "\n" . $this->displayInheritedRolesOrPermissionsAsHtml(false);
    }

    /**
     * Display inherited roles and permissions.
     *
     * @param bool $showRole
     * @return string
     */
    protected
    function displayInheritedRolesOrPermissionsAsHtml($showRole)
    {
        $r = '<ul class="';
        if ($showRole) {
            $r .= "roles";
        } else {
            $r .= "permissions";
        }
        $r .= '">';

        $parentId = OrganizationUnitInfo::getInstance()->getParentId($this->unitId, null);
        while (!is_null($parentId)) {
            $isTherePerm = false;
            $r1 = '';

            if ($showRole) {
                $relations = OrganizationUnitInfo::getInstance()->getDirectUsersWithRoles($parentId);
            } else {
                $relations = OrganizationUnitInfo::getInstance()->getDirectUsersWithPermissions($parentId);
            }

            foreach ($relations as $userId => $capabilities) {
                foreach ($capabilities as $capabilityId) {

                    $isTherePerm = true;

                    if ($showRole) {
                        $role = ArRolePeer::retrieveByPK($capabilityId);
                        $descr = ' has role ' . htmlspecialchars($role->getName() . ' ', ENT_QUOTES, 'UTF-8');
                    } else {
                        $permission = ArPermissionPeer::retrieveByPK($capabilityId);
                        $descr = htmlspecialchars($permission->getName() . ' ', ENT_QUOTES, 'UTF-8');
                    }
                    $user = ArUserPeer::retrieveByPK($userId);
                    $r1 .= '<li>' . __('user') . ' ' . $user->getNameAsHtml() . ' ' . $descr . '</li>';
                }

                if ($isTherePerm) {
                    $r .= '<br/>' . __('inherited from') . ' ' . OrganizationUnitInfo::getInstance()->getFullNameAtDate($parentId, null, true, true) . '<ul>' . $r1 . '</ul>';
                }

            }

            $r .= '</ul>';
            $parentId = OrganizationUnitInfo::getInstance()->getParentId($parentId, null);
        }
        return $r;
    }


//////////////////////////////
// Displaying Unit Hiearchy //
//////////////////////////////

    /**
     * @param int[] $dates modification dates
     * @return string
     */
    protected
    function createAListOfChangeAsHtml($dates)
    {

        $r = '<ul>';
        foreach ($dates as $date) {
            if (!is_null($date)) {
                $r .= '<li>' . OrganizationUnitInfo::getInstance()->getShortDescription($this->unitId, $date, true) . '</li>';
            }
        }

        $r .= '</ul>';

        return $r;
    }


    /**
     * Used internally from `getChildrenAsHTMLList`
     */
    private
        $htmlChildCounter = 0;

    /**
     * @param int $rootId
     * @param int|null $date
     * @param bool $alsoLeaf
     * @return string the content
     */
    protected
    function getChildrenAsHTMLList($rootId, $date, $alsoLeaf = true)
    {

        $this->htmlChildCounter = 1;

        return '<ul class="tree">' . $this->getChildrenAsHTMLListRec($rootId, $date, $alsoLeaf, true) . '</ul>';
    }

    /**
     * Internal function
     *
     * @param int $rootId
     * @param int|null $date
     * @param bool $alsoLeaf
     * @return string the content
     */
    protected
    function getChildrenAsHTMLListRec($rootId, $date, $alsoLeaf, $isRoot)
    {

        // Show Node Name


        $rootInfo = OrganizationUnitInfo::getInstance()->getDataInfo($rootId, $date);

        if (is_null($rootInfo) || !$rootInfo[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS]) {
            return "";
        }

        if ($isRoot) {
            $rootName = OrganizationUnitInfo::getInstance()->getFullNameAtDate($rootId, $date, true, true);
        } else {
            $rootName = OrganizationUnitInfo::getInstance()->getShortName($rootId, $date, true);
        }

        $r = '<li>' . $rootName;

        $children = OrganizationUnitInfo::getInstance()->getDirectChildrenAtDate($rootId, $date, $alsoLeaf);

        // Show extensions (if there are) in a compact way

        $isThereResult1 = false;
        $r1 = '<li>Extensions: ';
        foreach ($children as $childId => $ignore) {
            if (OrganizationUnitInfo::getInstance()->getTypeIsLeaf($childId, $date)) {
                if ($isThereResult1) {
                    $r1 .= ', ';
                } else {
                    $isThereResult1 = true;
                }

                $r1 .= OrganizationUnitInfo::getInstance()->getShortName($childId, $date, true, false);
            }
        }
        $r1 .= '</li>';

        // Show Expandible Children (if there are)

        $isThereResult2 = false;
        $r2 = '';
        foreach ($children as $childId => $ignore) {
            if (!OrganizationUnitInfo::getInstance()->getTypeIsLeaf($childId, $date)) {
                $isThereResult2 = true;
                $r2 .= $this->getChildrenAsHTMLListRec($childId, $date, $alsoLeaf, false);
            }
        }


        if ($isThereResult1 || $isThereResult2) {
            $r .= '<ul>';

            if ($isThereResult1) {
                $r .= $r1;
            }

            if ($isThereResult2) {
                $r .= $r2;
            }

            $r .= '</ul>';
        }

        $r .= '</li>';

        return $r;
    }

    /**
     * @param int $rootId
     * @param int|null $date
     * @param bool $alsoLeaf
     * @return string the content
     */
    protected
    function getChildrenChangesAsHTMLList($rootId, $date, $alsoLeaf = true)
    {
        return '<ul class="tree">' . $this->getChildrenChangesAsHTMLListRec($rootId, $date, $alsoLeaf, true) . '</ul>';
    }

    /**
     * Internal function
     * @param int $rootId
     * @param int|null $date
     * @param bool $alsoLeaf
     * @return string the content
     */
    protected
    function getChildrenChangesAsHTMLListRec($rootId, $date, $alsoLeaf, $isRoot)
    {
        $rootInfo = OrganizationUnitInfo::getInstance()->getDataInfo($rootId, $date);

        if (is_null($rootInfo)) {
            return "";
        }

        if ($isRoot) {
            $rootName = OrganizationUnitInfo::getInstance()->getFullNameAtDate($rootId, $date, true, true);
        } else {
            $rootName = OrganizationUnitInfo::getInstance()->getShortName($rootId, $date, true);
        }

        $r = '<li>' . $rootName . '<ul>';

        // Put direct changes
        $changes = OrganizationUnitInfo::getInstance()->getDirectPastModificationDates($rootId, $date);
        if (count($changes) > 0) {
            $r .= '<li>' . 'changes';
            $r .= '<li>';
            foreach ($changes as $change) {
                $r .= '<li>' . OrganizationUnitInfo::getInstance()->getShortDescription($rootId, $change, true) . '</li>';
            }
            $r .= '</ul>';
            $r .= '</li>';
        }

        // Put children changes
        $children = OrganizationUnitInfo::getInstance()->getDirectChildrenAtDate($rootId, $date, $alsoLeaf);
        if (count($children) > 0) {
            foreach ($children as $childId => $ignore) {
                $r .= $this->getChildrenChangesAsHTMLListRec($childId, $date, $alsoLeaf, false);
            }
        }
        $r .= '</ul>';
        $r .= '</li>';

        return $r;
    }
}


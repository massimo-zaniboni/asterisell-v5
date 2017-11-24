<?php use_helper('Asterisell', 'Url', 'I18N');

/**
 * @var int $unitId
 */

/**
 * @var int $calcFilterDate
 */

/**
 * @var array $unitInfo
 */

/**
 * @var ArOrganizationUnitHasStructure $currentOrganizationUnitHasStructure
 */

/**
 * @var string $unitInfoShortName
 */

/**
 * @var int $filterDate
 */

/**
 * @var bool $isThereFilterDate
 */

/**
 * @var bool $canDeleteOrganization
 */

/**
 * @var ArNewProblem[] $unitProblems
 */

?>

<div id="sf_admin_container">

<h1><?php echo $sf_data->getRaw('unitInfoShortName'); ?></h1>

<div id="sf_admin_header">
</div>

<div id="sf_admin_content">

    <form id="sf_admin_edit_form" name="sf_admin_edit_form" method="post" enctype="multipart/form-data"
          action=<?php echo '"' . $sf_data->getRaw('buttonSelectionUrl') . '"'; ?>>

        <?php if (count($unitProblems) > 0) { ?>
            <div class="form-errors">
                <dl>
                    <?php foreach ($unitProblems as $p) {

                        if (is_null($p)) {
                            ?>
                            <dt>Problem:</dt>
                            <dd>Some part of customers and organizations info is corrupted.</dd>
                            <dt>Effect:</dt>
                            <dd>You can not be sure that the application behaviour is correct, because data is
                                corrupt.
                            </dd>
                            <dt>Proposed Solution:</dt>
                            <dd>Check Status->Current Problems form, and correct the problems regarding customers and organizations info. If the error persist, restore data from backup, or contact the assistance.</dd>
                            <br/>
                        <?php
                        } else {
                            ?>
                            <dt>Problem:</dt>
                            <dd><?php echo htmlentities($p->getDescription()); ?></dd>
                            <dt>Effect:</dt>
                            <dd><?php echo htmlentities($p->getEffect()); ?></dd>
                            <dt>Proposed Solution:</dt>
                            <dd><?php echo htmlentities($p->getProposedSolution()); ?></dd>
                            <br/>
                        <?php
                        }
                    } ?>
                </dl>
            </div>
        <?php } ?>

        <fieldset class="">

            <?php if ($unitInfo[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF]) { ?>

                <h2>Extension</h2>

                <div class="form-row">
                    <label><?php echo __("Name"); ?></label>

                    <div class="content">
                        <?php echo $unitInfoShortName; ?>
                    </div>
                </div>

                <div class="form-row">
                    <label><?php echo __("Extension codes"); ?></label>

                    <div class="content">
                        <?php echo $currentOrganizationUnitHasStructure->getExtensionCodes(); ?>
                    </div>
                </div>

                <div class="form-row">
                    <label><?php echo __("Export Code"); ?></label>

                    <div class="content">
                        <?php echo input_tag('export_code', $exportCode, array('size' => 30)); ?>
                        <input type="submit" name="save_export_code" value="Save"/>

                        <div class="sf_admin_edit_help">In case the CDR is exported to a reseller, this unique code
                            identifies the extension associated to the call on the reseller side. All CDR associated to
                            this extension, are exported to the reseller, as calls associated to this `export code`.
                            Then the reseller must use this code as `extension code`.
                        </div>
                    </div>
                </div>

            <?php } else { ?>
                <h2>Organization</h2>

                <div class="form-row">
                    <label><?php echo __("Party"); ?></label>

                    <div class="content">
                        <?php   $t = OrganizationUnitInfo::getInstance()->getLinkToAssociatedEntity($unitId, $calcFilterDate);
                        if (!is_null($t)) {
                            echo '<a href="' . url_for($t) . '">' . $unitInfoShortName . '</a>';
                        } else {
                            echo $unitInfoShortName;
                        }
                        ?>
                    </div>
                    <div class="sf_admin_edit_help">Use the link for changing party info. Party info is always related
                        to the current status of the party, and older versions are not maintained.
                    </div>

                </div>

                <div class="form-row">
                    <label><?php echo __("Is Billable?"); ?></label>

                    <div class="content">
                        <?php if ($unitInfo[OrganizationUnitInfo::DATA_UNIT_IS_BILLABLE]) {
                            echo __('Yes: the organization can be billed (and it is fiscally responsible) for all the costs of his direct and indirected extensions.');
                        } else {
                            echo __('No: the organization costs are passed to its first billable parent, because this organization is not fiscally responsible for them.');
                        }
                        ?>
                    </div>
                </div>

                <div class="form-row">
                    <label><?php echo __("Export Code"); ?></label>

                    <div class="content">
                        <?php echo input_tag('export_code', $exportCode, array('size' => 30)); ?>
                        <input type="submit" name="save_export_code" value="Save"/>

                        <div class="sf_admin_edit_help">In case this instance export CDRS of this organization to
                            another Asterisell instance acting like a reseller, this unique code identifies the
                            organization on the reseller side. This code is used for associating services associated to
                            this organization, to the corresponding service costs on the reseller side.
                        </div>
                    </div>
                </div>

            <?php
            }
            ?>

            <div class="form-row">
                <label>Configure using YAML file</label>

                <div class="content">
                    <input type="submit" name="export_all_to_yaml" value="Export all Organizations"/>
                    <input type="submit" name="export_to_yaml" value="Export this Organization"/>
                    <?php echo input_file_tag('yaml_file', array('size' => 30)); ?>
                    <input type="submit" name="import_from_yaml" value="Import"/>

                    <div class="sf_admin_edit_help">Allows to configure organizations and extensions using a text
                        editor, instead of the WEB User Interface. Usually you should first select the part of
                        organization you want change, export as YAML view, modify the YAML file, and then import it. The
                        import button work according the content of the YAML file, and not depend from the selected
                        organization in the web form.
                    </div>
                </div>
            </div>

            <div class="form-row">
                <label>Organization Hierarchy</label>
                <?php
                $addOrganizationOrExtensionDscr = 'A child organization is an organization that is part of this organization. An extension can be a classic (POTS) or SIP (VOIP) telephone line/number. For associating calls to a user, there must be at least an organization with a child extension. These buttons add a generic organization or extension to the current organization. The organization/extension will be created using default values. Then you must use the fix button for completing with correct values.';
                if ($unitInfo[OrganizationUnitInfo::DATA_UNIT_TYPE_IS_LEAF]) {
                    ?>
                    <div class="content">
                        <input type="submit" name="add_child_organization" value="Add Child Organization" disabled/>
                        <input type="submit" name="add_child_extension" value="Add Child Extension" disabled/>

                        <div class="sf_admin_edit_help"><?php echo $addOrganizationOrExtensionDscr; ?></div>
                    </div>
                <?php
                } else {
                ?>
                <div class="content">
                    <input type="submit" name="add_child_organization" value="Add Child Organization"/>
                    <input type="submit" name="add_child_extension" value="Add Child Extension"/>

                    <div class="sf_admin_edit_help"><?php echo $addOrganizationOrExtensionDscr; ?></div>
                </div>
            </div>
        <?php
        }
        ?>

</div>

<?php if ($unitInfo[OrganizationUnitInfo::DATA_STRUCTURE_EXISTS]) { ?>
    <div class="form-row">
        <label><?php echo htmlentities(__("Organization Hierarchy")); ?> </label>

        <div class="content">
            <?php echo $sf_data->getRaw('unitInfoHtmlHierarchy'); ?>
        </div>
    </div>
<?php } ?>

</fieldset>

<fieldset class="">
    <?php if ($isThereFilterDate) {
        echo '<h2>Properties at ' . htmlentities(fromUnixTimestampToSymfonyStrTimestamp($filterDate)) . '</h1>';
    } else {
        echo '<h2>Current Properties</h1>';
    }?>

    <?php if ($isThereFilterDate) { ?>
        <div class="form-row">
            <label></label>

            <div class="content">
                <?php echo "<a href='" . $sf_data->getRaw('linkToCurrentView') . "'>Go to current view</a>"; ?>
            </div>
        </div>
    <?php } ?>

    <div class="form-row">
        <label>Current Description</label>

        <div class="content">
            <?php echo $sf_data->getRaw('lastClassificationDescription'); ?>
        </div>
    </div>

    <?php if ($unitInfo[OrganizationUnitInfo::DATA_ORGANIZATION_MANAGED_FROM] != ArOrganizationUnit::MANAGED_FROM_USER) { ?>

        <div class="form-row">
            <label>Management</label>

            <div class="content">
                This organization/extension is managed from system jobs. If you change some of its properties, they can
                be overwritten, at next execution of these jobs. So the preferred way is changing the properties
                directly using the source data repository, from where the automatic jobs retrieve data.
            </div>
        </div>
    <?php } ?>

    <div class="form-row">
        <label>Change</label>

        <div class="content">
            <input type="submit" name="classification_fix" value="Fix"/>
            <input type="submit" name="classification_delete" value="Delete"/>

            <div class="sf_admin_edit_help">
               Use this button for fixing errors, but only if you are configuring a new organization/extension,
               and there are no old billed CDRS associated to it.
               Current CDRS will be rated again, with the new fixed settings.
               Do not use this button, if there are already billed CDRS associated to the extension,
               because you could corrupt them.
               Use this button also for fixing errors in last inserted changes, because
               they affect only CDRS yet to bill (that will be calculated again).
            </div>
        </div>
    </div>

    <div class="form-row">
        <label>Add to history</label>

        <div class="content">
            <input type="submit" name="change_from_date" value="Change From Date"/>
            <input type="submit" name="disable_from_date" value="Disable From Date"/>

            <div class="sf_admin_edit_help">
                Use this button when there is a change in the past, current or future configurations of the
                organization/extension. The new settings will be added to the history of changes
                of the organization/extensions, and old settings can still be inspected.
                Current CDRS will be rated again: CDRS before the date of change will be rated using the old settings,
                CDRS after the date of change will be rated with the new settings.
                If you committed errors in specifying last settings, you can fix them using the Fix Button,
                instead of this button, but only if the changes are not part of already billed CDRS.
            </div>
        </div>
    </div>
</fieldset>

<fieldset class="">
    <?php if ($isThereFilterDate) {
        echo '<h2>Permissions at ' . htmlentities(fromUnixTimestampToSymfonyStrTimestamp($filterDate)) . '</h1>';
    } else {
        echo '<h2>Current Permissions</h1>';
    }?>

    <div class="form-row">
        <label><?php echo htmlentities(__("Roles")); ?></label>

        <div class="content">
            <?php echo $sf_data->getRaw('directRoles'); ?>
        </div>
    </div>

    <div class="form-row">
        <label><?php echo htmlentities(__("Permissions")); ?></label>

        <div class="content">
            <?php echo $sf_data->getRaw('directPermissions'); ?>
        </div>
    </div>

    <div class="form-row">
        <label>                <?php echo htmlentities(__("Parent Permissions")); ?>
        </label>

        <div class="content">
            <?php echo $sf_data->getRaw('inheritedPermissions'); ?>
        </div>
    </div>

</fieldset>


<fieldset class="">
    <h2>Past Properties</h2>

    <div class="form-row">
        <label> <?php echo htmlentities(__("Direct children in the past")); ?></label>

        <div class="content">
            <?php echo $sf_data->getRaw('htmlListOfDirectChildrenInThePast');
            ?>
        </div>
    </div>

    <div class="form-row">
        <label><?php echo htmlentities(__("Classifications in the past")); ?>
        </label>

        <div class="content">
            <?php echo $sf_data->getRaw('htmlListOfDirectChanges');
            // TODO put jump to change date button
            ?>
        </div>
    </div>

</fieldset>

<fieldset class="">
    <h2>Future Properties</h2>

    <div class="form-row">
        <label><?php echo htmlentities(__("Direct children in the future")); ?> </label>

        <div class="content">
            <?php echo $sf_data->getRaw('htmlListOfDirectChildrenInTheFuture');
            ?>
        </div>
    </div>

    <div class="form-row">
        <label><?php echo htmlentities(__("Classifications in the future")); ?></label>

        <div class="content">
            <?php echo $sf_data->getRaw('htmlListOfDirectChangesInTheFuture');
            ?>
        </div>
    </div>
</fieldset>

<ul class="sf_admin_actions">
    <li><input class="sf_admin_action_list" value="list" type="button"
               onclick=<?php echo '"document.location.href=\'' . url_for('root_organizations/list') . '\';"' ?>
            />
    </li>
</ul>
</form>

<ul class="sf_admin_actions">
    <li class="float-left">
        <form
            class="button_to" <?php echo 'action="' . url_for('organization_full_view/delete?id=' . $unitId) . '" method="post"' ?> >
            <div>
            </div>
        </form>
    </li>
</ul>

</div>
</div>

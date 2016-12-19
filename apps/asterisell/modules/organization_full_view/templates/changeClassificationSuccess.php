<?php

/**
 * @var ArOrganizationUnitHasStructureForm $form
 */
?>

<div id="sf_admin_container">
    <h1>Organization Classification</h1>

    <div id="sf_admin_header">
    </div>

    <div id="sf_admin_content">

        <form action="<?php echo $sf_data->getRaw('changeClassificationUrl'); ?>" method="post" id="sf_admin_edit_form" name="sf_admin_edit_form">

            <fieldset class="">
                    <h2>Type of Change</h2>
                    <div class="form-row">
                        <?php $fn = 'from'; echo $form[$fn]->renderLabel() ?>
                        <div class="content">
                            <?php echo $form[$fn]->render() ?>
                            <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                        </div>
                    </div>

                <div class="form-row">
                    <?php $fn = 'exists'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

                <div class="form-row">
                    <?php $fn = ArOrganizationUnitHasStructureForm::TYPE_OF_CHANGE; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

            </fieldset>

            <fieldset class="">
                <h2>Hierarchy</h2>
                <div class="form-row">
                    <?php $fn = 'ar_organization_unit_type_id'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

                <div class="form-row">
                    <?php $fn = 'ar_rate_category_id'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

                <div class="form-row">
                    <?php $fn = 'ar_parent_organization_unit_id'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

            </fieldset>

            <fieldset class="">
                <h2>Complete only in case of an Organization</h2>
                <div class="form-row">
                    <?php $fn = 'ar_party_id'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

            </fieldset>

            <fieldset class="">
                <h2>Complete only in case of an Extension</h2>

                <div class="form-row">
                    <?php $fn = 'extension_codes'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

                <div class="form-row">
                    <?php $fn = 'extension_name'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

                <div class="form-row">
                    <?php $fn = 'extension_user_code'; echo $form[$fn]->renderLabel() ?>
                    <div class="content">
                        <?php echo $form[$fn]->render() ?>
                        <div class="sf_admin_edit_help"><?php echo $form[$fn]->renderHelp() ?></div>
                    </div>
                </div>

            </fieldset>

            <?php echo $form->renderHiddenFields() ?>

            <ul class="sf_admin_actions">
                <li><input class="sf_admin_action_list" name="cancel" value="Cancel" type="submit"/></li>
                <li><input type="submit" name="save" value="Confirm" class="sf_admin_action_save"/></li>
            </ul>

        </form>
    </div>
</div>

<?php
use_helper('Number', 'I18N', 'Date', 'OnlineManual', 'Asterisell');

$params = ArParamsPeer::getDefaultParams();

function toManualPage($moduleName, $base) {
    $arr = array(
        'admin_call_report' => 'call_report',
        'communication_channel' => 'communication-channel-type',
        'vendor_domain' => 'communication-channel',
        'backup_of_organizations' => 'backup_of_organizations',
        'holidays' => 'holidays',
        'number_portability' => 'number_portability',
        'party' => 'party',
        'user' => 'user',
        'problem' => 'problem',
        'rate' => 'rate',
        'report' => 'reports-2',
        'report_scheduling' => 'reports-2',
        'report_set' => 'reports-2',
        'root_organizations' => 'root_organizations',
        'organization_full_view' => 'root_organizations',
        'service_price_list' => 'service_price_list',
        'service_sale' => 'service_sale',
        'tag' => 'tag'
    );
    if (array_key_exists($moduleName, $arr)) {
        $helpPage = $arr[$moduleName];
        return '<a href="' . $base . '/admin/manual/' . $helpPage . '.html" target="_blank">' . __('Help about this Module') . '</a>';
    } else {
        return '<a href="' . $base . '/admin/manual/manual.html" target="_blank">' . __('Missing Help about this Module') . '</a>';
    }
}

?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <?php echo include_http_metas() ?>
    <?php echo include_metas() ?>
    <?php include_javascripts() ?>
    <?php include_stylesheets() ?>

    <title> <?php echo $params->getServiceName() ?> </title>

</head>

<body>

<div id="userInfoBlock">

    <?php
// Login and LOGO
//
    echo '<table width="100%">';
    echo '<tr>';
    echo '<td>';
    echo image_tag($params->getLogoImage());
    echo '</td>';
    echo '<td> <p class="loginData">';
    if ($sf_user->isAuthenticated()) {
        echo __('User:') . ' ' ;
        if ($sf_user->isAdmin()) {
            echo $sf_user->getLoginDescription();
        } else {
            echo link_to($sf_user->getLoginDescription(), 'viewuser/index');
        }
        echo ' - ' . link_to(__('Logout'), 'login/logout');
    }
    echo '</p></td>';
    echo '</tr>';
    echo '<tr>';
    echo '<td>';
    echo '<p class="asterisellSlogan">' . $params->getSlogan() . '</p>';
    echo '</td>';
    echo '</tr>';
    echo '</table>';
    ?>

    <?php

// Menu
//
    if ($sf_user->hasCredential('admin')) {

        $base = sfConfig::get('app_instance_url_path');
        if ($base !== '') {
            if (substr($base, 0, 1) !== '/') {
                $base = '/' . $base;
            }
        }

        echo "\n";
        echo '<div class="appmenu">' . "\n";
        echo '<ul>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Params') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . link_to(__('Params'), 'params/edit?id=' . ArParamsPeer::getDefaultParamsId()) . '</li>' . "\n";
        echo '<li>' . link_to(__('Upload Files'), 'sfMediaLibrary/index') . '</li>' . "\n";
        echo '<li>' . link_to(__('CDRs Providers'), 'cdr_provider/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Communication Channels'), 'communication_channel/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Telephone Prefixes'), 'telephone_prefix/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Holidays'), 'holidays/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Account Roles'), 'role/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Organization Types'), 'organization_type/list') . '</li>' . "\n";
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Rates') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . link_to(__('Number Portability'), 'number_portability/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Vendors'), 'vendor/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Channels'), 'vendor_domain/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Price Categories'), 'rate_category/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Rate Formats'), 'rate_formats/index') . '</li>' . "\n";
        echo '<li>' . link_to(__('Rates'), 'rate/list') . '</li>' . "\n";
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Entities') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . link_to(__('Customers'), 'root_organizations/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Parties'), 'party/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Party TAGS'), 'tag/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Backup'), 'backup_of_organizations/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Web Accounts'), 'user/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Resellers'), 'reseller/index') . '</li>' . "\n";
        echo '<li>' . link_to(__('Complete Info'), 'get_extension_list/exportToCsv') . '</li>' . "\n";
        // TODO temporary disabled
        //  echo '<li>' . link_to(__('Show Documents to Customers'), 'document/list') . '</li>'. "\n";
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Services') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . link_to(__('Services'), 'service/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Service Prices'), 'service_price_list/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Service Sales'), 'service_sale/list') . '</li>' . "\n";
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Reports') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . link_to(__('All Reports'), 'report/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Scheduled Reports'), 'report_scheduling/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Reports Sets'), 'report_set/list') . '</li>' . "\n";
        if (isVoIPResellerInstance()) {
          echo '<li>' . link_to(__('Fiscal Documents'), 'fiscal_document/list') . '</li>' . "\n";
        }
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Status') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . link_to(__('Calls'), 'admin_call_report/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Jobs Log'), 'jobqueue/list') . '</li>' . "\n";
        echo '<li>' . link_to(__('Rating Problems'), 'problem/list') . '</li>' . "\n";
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '<li><a href="" target="_self" >' . __('Help') . '</a>' . "\n";
        echo '<ul>' . "\n";
        echo '<li>' . toManualPage($sf_context->getModuleName(), $base) . '</li>' . "\n";
        echo '<li><a href="' . $base . '/admin/manual/manual.html" target="_blank">' . __('Manual') . '</a></li>' . "\n";
        echo '<li>' . link_to(__('Customizations'), 'about/customizations') . '</li>' . "\n";
        echo '<li>' . link_to(__('Version'), 'about/version') . '</li>' . "\n";
        echo '<li>' . link_to(__('License'), 'about/license') . '</li>' . "\n";
        echo '</ul>' . "\n";
        echo '</li>' . "\n";

        echo '</ul>' . "\n";
        echo '</div>' . "\n";
    } else if ($sf_user->hasCredential('user')) {
      // no menu to display
    }
    ?>

</div>

<hr/>

<?php
if ($sf_user->hasFlash('error')) {
    echo '<div class="form-errors">';
    echo '<h2>' . $sf_user->getFlash('error') . '</h2>';
    echo '</div>';
} else if ($sf_user->hasFlash('notice')) {
    echo '<div class="save-ok">';
    echo '<h2>' . $sf_user->getFlash('notice') . '</h2>';
    echo '</div>';
}
?>

<?php echo $sf_content ?>

<div id="asterisellFooter">
    <hr/>
    <?php echo $params->getFooter(ESC_RAW); ?>
</div>

</body>
</html>

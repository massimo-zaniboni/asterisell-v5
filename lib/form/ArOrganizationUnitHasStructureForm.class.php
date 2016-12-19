<?php

/**
 * ArOrganizationUnitHasStructure form.
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
class ArOrganizationUnitHasStructureForm extends BaseArOrganizationUnitHasStructureForm
{
  const ADD_CHANGE = 1;

  const FIX_ERROR = 2;

  const DELETE = 3;

  const TYPE_OF_CHANGE = 'type_of_change';

  public function configure()
  {
      $this->setWidget('ar_organization_unit_id', new sfWidgetFormInputHidden());

      $organizations = OrganizationUnitInfo::getInstance()->getUIOrganizationSelector(null, true, false, false);
      $this->setWidget('ar_parent_organization_unit_id', new sfWidgetFormSelect(array(
          'choices' => $organizations)
      ));

      $this->setWidget(self::TYPE_OF_CHANGE, new sfWidgetFormSelect(array(
          'choices'  => array(self::ADD_CHANGE => 'add to history',
                              self::FIX_ERROR => 'fix history',
                              self::DELETE => 'delete from history'),
          'default' => self::ADD_CHANGE
      )));

      $parties = ArPartyPeer::getPartiesSelector();
      $this->setWidget('ar_party_id', new sfWidgetFormSelect(array(
              'choices' => $parties)
      ));

      $this->setWidget('extension_codes', new sfWidgetFormInputText(array(), array(
              'size' => 70)
      ));

      $this->setWidget('extension_name', new sfWidgetFormInputText(array(), array(
              'size' => 70)
      ));

      $this->setWidget('extension_user_code', new sfWidgetFormInputText(array(), array(
              'size' => 70)
      ));


      $culture = trim(sfConfig::get('app_culture'));

      // Create an array with a lot of years, otherwise there can be an error in some usage of the widget
      $years = array();
      $y = date('Y');
      for($i = -50; $i < 30; $i++) {
          $year = $y + $i;
          $years[$year] = $year;
      }

      $this->setWidget('from', new sfWidgetFormI18nDateTime(array(
          'culture' => $culture,
          'date' => array('years' => $years)
      )));

      $this->widgetSchema->setLabels(array(
          'ar_parent_organization_unit_id' => __('Parent'),
          'ar_organization_unit_type_id' => __('Type'),
          'from' => __('From'),
          'exists' => __('Is active'),
          'ar_rate_category_id' => __('Price Category'),
          'ar_party_id' => __('Party'),
          'extension_name' => __('Extension Name'),
          'extension_user_code' => __('Extension Short Name'),
          'extension_codes' => __('Extension Codes')
      ));

      $this->widgetSchema->setHelps(array(
          'ar_party_id' => __('The party associated to the organization, containing all the information about the organization. Note: party contains existential info, so if you change info on the party, the old values are not preserved.'),
          'extension_name' => __('An optional name of the extension to show in the call report. Leave empty if the used extension is not important for the user.  In this case the call made from the extension are displayed as calls of the owner organization unit, and the details on the used extension are not displayed.'),
          'extension_user_code' => __('An optional human readable short name. For example in "John Smith (5321)", "John Smith" is the extension name, and "5321" is the extension short name/code. Empty for not showing this info.'),
          'extension_codes' => __('Unique internal extension codes, used in the CDRs for identifying this extension. Extension codes can be separated by comma ",", for example "1212,1222,1244". "123X" matches the telephone number "123" followed by one character. "123\X" matches exactly the "123X" telephone number, with the explicit "X" character. "123XX" matches telephone numbers starting with "123" followed by two characters. "123*" match "123" followed by zero or more characters. "123X*" match "123" followed by one or more characters. "123XX3*" matches "123" followed by 2 chars, then "3" and zero or more characters. "123X*35" is an invalid format. "123\X\*35" matches exactly "123X*35". "123" has more priority respect the pattern "12X".'),
          'ar_parent_organization_unit_id' => __('The parent/owner organization of this organization/extension. Empty if this is a root organization/customer.'),
          'from' => __('From when these settings will take effect. In case of a price category associated to a bundle-rate, the date of activation of the price category, correspond to the date of activation of the bundle-rate.'),
          'exists' => __('Disabled (false) if the organization/extension does not exist anymore, from the specified date.'),
          'ar_rate_category_id' => __('Empty for inheriting the same price category of the owner. In case of bundle-rates, set explicitly to the bundle-rate price category for applying the associated costs and limits to this organization, without using the bundle-rate costs and limits of its parent organization, or leave empty if the calls of this organization must be accounted to the bundle-rate costs and limits of its parent organization.'),
          'ar_organization_unit_type_id' => __('The type of the organization. This field is only used for describing the organization, and it has no other special meaning.'),
          self::TYPE_OF_CHANGE => __('"fix history": for replacing current properties with a correct version. "delete from history": for removing current properties from history. "add to history": for setting new properties becoming active from the specified date, preserving old properties in the past.')
      ));

      // NOTE: the display order is hard-coded into changeClassificationSuccess.php template

      $this->getWidgetSchema()->moveField('ar_organization_unit_type_id', sfWidgetFormSchema::FIRST);

      $this->getWidgetSchema()->moveField('from', sfWidgetFormSchema::AFTER, 'ar_organization_unit_type_id');
      $this->getWidgetSchema()->moveField(self::TYPE_OF_CHANGE, sfWidgetFormSchema::AFTER, 'from');
      $this->getWidgetSchema()->moveField('exists', sfWidgetFormSchema::AFTER, self::TYPE_OF_CHANGE);

      $this->getWidgetSchema()->moveField('ar_parent_organization_unit_id', sfWidgetFormSchema::AFTER, 'exists');

      $this->getWidgetSchema()->moveField('ar_rate_category_id', sfWidgetFormSchema::AFTER, 'extension_codes');
      $this->getWidgetSchema()->moveField('ar_party_id', sfWidgetFormSchema::AFTER, 'ar_parent_organization_unit_id');

      $this->getWidgetSchema()->moveField('extension_codes', sfWidgetFormSchema::AFTER, 'ar_party_id');
      $this->getWidgetSchema()->moveField('extension_name', sfWidgetFormSchema::AFTER, 'extension_user_code');
      $this->getWidgetSchema()->moveField('extension_user_code', sfWidgetFormSchema::AFTER, 'extension_name');

      $this->validatorSchema->setOption('allow_extra_fields', true);
      $this->validatorSchema->setOption('filter_extra_fields', false);

      $this->widgetSchema->addFormFormatter('like_admin_generator', new sfWidgetFormSchemaFormatterAdminGenerator($this->widgetSchema));
      $this->widgetSchema->setFormFormatterName('like_admin_generator');

  }
}

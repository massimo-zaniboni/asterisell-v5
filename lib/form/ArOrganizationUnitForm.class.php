<?php

/**
 * ArOrganizationUnit form.
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
class ArOrganizationUnitForm extends BaseArOrganizationUnitForm
{
  public function configure()
  {
       $this->widgetSchema->setLabels(array(
          'internal_name' => __('Internal Code'),
       ));

      $this->widgetSchema->setHelps(array(
          'internal_name' => __('A unique, internal code, used as reference for the extension.')
      ));

      $this->getWidgetSchema()->moveField('internal_name', sfWidgetFormSchema::FIRST);
  }
}

<?php

/**
 * ArItcOrganizationsDebugCopy filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArItcOrganizationsDebugCopyFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'last_update' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'org'         => new sfWidgetFormFilterInput(),
      'name'        => new sfWidgetFormFilterInput(),
      'description' => new sfWidgetFormFilterInput(),
      'accountcode' => new sfWidgetFormFilterInput(),
      'parent'      => new sfWidgetFormFilterInput(),
      'email'       => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'last_update' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'org'         => new sfValidatorPass(array('required' => false)),
      'name'        => new sfValidatorPass(array('required' => false)),
      'description' => new sfValidatorPass(array('required' => false)),
      'accountcode' => new sfValidatorPass(array('required' => false)),
      'parent'      => new sfValidatorPass(array('required' => false)),
      'email'       => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_itc_organizations_debug_copy_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcOrganizationsDebugCopy';
  }

  public function getFields()
  {
    return array(
      'id'          => 'Number',
      'last_update' => 'Date',
      'org'         => 'Text',
      'name'        => 'Text',
      'description' => 'Text',
      'accountcode' => 'Text',
      'parent'      => 'Text',
      'email'       => 'Text',
    );
  }
}

<?php

/**
 * ArItcInputExtensionsDebugCopy filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArItcInputExtensionsDebugCopyFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'last_update'             => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'extension_code'          => new sfWidgetFormFilterInput(),
      'dial_string'             => new sfWidgetFormFilterInput(),
      'name'                    => new sfWidgetFormFilterInput(),
      'accountcode'             => new sfWidgetFormFilterInput(),
      'accountcode_description' => new sfWidgetFormFilterInput(),
    ));

    $this->setValidators(array(
      'last_update'             => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'extension_code'          => new sfValidatorPass(array('required' => false)),
      'dial_string'             => new sfValidatorPass(array('required' => false)),
      'name'                    => new sfValidatorPass(array('required' => false)),
      'accountcode'             => new sfValidatorPass(array('required' => false)),
      'accountcode_description' => new sfValidatorPass(array('required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_itc_input_extensions_debug_copy_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArItcInputExtensionsDebugCopy';
  }

  public function getFields()
  {
    return array(
      'id'                      => 'Number',
      'last_update'             => 'Date',
      'extension_code'          => 'Text',
      'dial_string'             => 'Text',
      'name'                    => 'Text',
      'accountcode'             => 'Text',
      'accountcode_description' => 'Text',
    );
  }
}

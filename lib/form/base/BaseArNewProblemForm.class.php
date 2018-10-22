<?php

/**
 * ArNewProblem form base class.
 *
 * @method ArNewProblem getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArNewProblemForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'duplication_key'           => new sfWidgetFormInputHidden(),
      'ar_problem_type_id'        => new sfWidgetFormPropelChoice(array('model' => 'ArProblemType', 'add_empty' => true)),
      'ar_problem_domain_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArProblemDomain', 'add_empty' => true)),
      'ar_problem_responsible_id' => new sfWidgetFormPropelChoice(array('model' => 'ArProblemResponsible', 'add_empty' => true)),
      'created_at'                => new sfWidgetFormDateTime(),
      'garbage_collection_key'    => new sfWidgetFormInputText(),
      'garbage_collection_from'   => new sfWidgetFormDateTime(),
      'garbage_collection_to'     => new sfWidgetFormDateTime(),
      'description'               => new sfWidgetFormTextarea(),
      'effect'                    => new sfWidgetFormTextarea(),
      'proposed_solution'         => new sfWidgetFormTextarea(),
      'signaled_to_admin'         => new sfWidgetFormInputCheckbox(),
      'count_of_cdrs'             => new sfWidgetFormInputText(),
    ));

    $this->setValidators(array(
      'duplication_key'           => new sfValidatorChoice(array('choices' => array($this->getObject()->getDuplicationKey()), 'empty_value' => $this->getObject()->getDuplicationKey(), 'required' => false)),
      'ar_problem_type_id'        => new sfValidatorPropelChoice(array('model' => 'ArProblemType', 'column' => 'id', 'required' => false)),
      'ar_problem_domain_id'      => new sfValidatorPropelChoice(array('model' => 'ArProblemDomain', 'column' => 'id', 'required' => false)),
      'ar_problem_responsible_id' => new sfValidatorPropelChoice(array('model' => 'ArProblemResponsible', 'column' => 'id', 'required' => false)),
      'created_at'                => new sfValidatorDateTime(array('required' => false)),
      'garbage_collection_key'    => new sfValidatorString(array('max_length' => 1024, 'required' => false)),
      'garbage_collection_from'   => new sfValidatorDateTime(array('required' => false)),
      'garbage_collection_to'     => new sfValidatorDateTime(array('required' => false)),
      'description'               => new sfValidatorString(array('required' => false)),
      'effect'                    => new sfValidatorString(array('required' => false)),
      'proposed_solution'         => new sfValidatorString(array('required' => false)),
      'signaled_to_admin'         => new sfValidatorBoolean(),
      'count_of_cdrs'             => new sfValidatorInteger(array('min' => -9.2233720368548E+18, 'max' => 9223372036854775807)),
    ));

    $this->widgetSchema->setNameFormat('ar_new_problem[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArNewProblem';
  }


}

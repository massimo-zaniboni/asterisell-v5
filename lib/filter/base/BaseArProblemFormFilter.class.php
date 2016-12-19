<?php

/**
 * ArProblem filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArProblemFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_problem_type_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArProblemType', 'add_empty' => true)),
      'created_at'              => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'duplication_key'         => new sfWidgetFormFilterInput(array('with_empty' => false)),
      'garbage_collection_key'  => new sfWidgetFormFilterInput(),
      'garbage_collection_from' => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'garbage_collection_to'   => new sfWidgetFormFilterDate(array('from_date' => new sfWidgetFormDate(), 'to_date' => new sfWidgetFormDate())),
      'description'             => new sfWidgetFormFilterInput(),
      'effect'                  => new sfWidgetFormFilterInput(),
      'proposed_solution'       => new sfWidgetFormFilterInput(),
      'signaled_to_admin'       => new sfWidgetFormChoice(array('choices' => array('' => 'yes or no', 1 => 'yes', 0 => 'no'))),
    ));

    $this->setValidators(array(
      'ar_problem_type_id'      => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArProblemType', 'column' => 'id')),
      'created_at'              => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'duplication_key'         => new sfValidatorPass(array('required' => false)),
      'garbage_collection_key'  => new sfValidatorPass(array('required' => false)),
      'garbage_collection_from' => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'garbage_collection_to'   => new sfValidatorDateRange(array('required' => false, 'from_date' => new sfValidatorDate(array('required' => false)), 'to_date' => new sfValidatorDate(array('required' => false)))),
      'description'             => new sfValidatorPass(array('required' => false)),
      'effect'                  => new sfValidatorPass(array('required' => false)),
      'proposed_solution'       => new sfValidatorPass(array('required' => false)),
      'signaled_to_admin'       => new sfValidatorChoice(array('required' => false, 'choices' => array('', 1, 0))),
    ));

    $this->widgetSchema->setNameFormat('ar_problem_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArProblem';
  }

  public function getFields()
  {
    return array(
      'id'                      => 'Number',
      'ar_problem_type_id'      => 'ForeignKey',
      'created_at'              => 'Date',
      'duplication_key'         => 'Text',
      'garbage_collection_key'  => 'Text',
      'garbage_collection_from' => 'Date',
      'garbage_collection_to'   => 'Date',
      'description'             => 'Text',
      'effect'                  => 'Text',
      'proposed_solution'       => 'Text',
      'signaled_to_admin'       => 'Boolean',
    );
  }
}

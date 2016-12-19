<?php

/**
 * ArProblemDefaultResponsible filter form base class.
 *
 * @package    asterisell
 * @subpackage filter
 * @author     Your name here
 */
abstract class BaseArProblemDefaultResponsibleFormFilter extends BaseFormFilterPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'ar_problem_domain_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArProblemDomain', 'add_empty' => true)),
      'ar_problem_responsible_id' => new sfWidgetFormPropelChoice(array('model' => 'ArProblemResponsible', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'ar_problem_domain_id'      => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArProblemDomain', 'column' => 'id')),
      'ar_problem_responsible_id' => new sfValidatorPropelChoice(array('required' => false, 'model' => 'ArProblemResponsible', 'column' => 'id')),
    ));

    $this->widgetSchema->setNameFormat('ar_problem_default_responsible_filters[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArProblemDefaultResponsible';
  }

  public function getFields()
  {
    return array(
      'id'                        => 'Number',
      'ar_problem_domain_id'      => 'ForeignKey',
      'ar_problem_responsible_id' => 'ForeignKey',
    );
  }
}

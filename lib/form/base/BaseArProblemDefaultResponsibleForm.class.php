<?php

/**
 * ArProblemDefaultResponsible form base class.
 *
 * @method ArProblemDefaultResponsible getObject() Returns the current form's model object
 *
 * @package    asterisell
 * @subpackage form
 * @author     Your name here
 */
abstract class BaseArProblemDefaultResponsibleForm extends BaseFormPropel
{
  public function setup()
  {
    $this->setWidgets(array(
      'id'                        => new sfWidgetFormInputHidden(),
      'ar_problem_domain_id'      => new sfWidgetFormPropelChoice(array('model' => 'ArProblemDomain', 'add_empty' => true)),
      'ar_problem_responsible_id' => new sfWidgetFormPropelChoice(array('model' => 'ArProblemResponsible', 'add_empty' => true)),
    ));

    $this->setValidators(array(
      'id'                        => new sfValidatorChoice(array('choices' => array($this->getObject()->getId()), 'empty_value' => $this->getObject()->getId(), 'required' => false)),
      'ar_problem_domain_id'      => new sfValidatorPropelChoice(array('model' => 'ArProblemDomain', 'column' => 'id', 'required' => false)),
      'ar_problem_responsible_id' => new sfValidatorPropelChoice(array('model' => 'ArProblemResponsible', 'column' => 'id', 'required' => false)),
    ));

    $this->widgetSchema->setNameFormat('ar_problem_default_responsible[%s]');

    $this->errorSchema = new sfValidatorErrorSchema($this->validatorSchema);

    parent::setup();
  }

  public function getModelName()
  {
    return 'ArProblemDefaultResponsible';
  }


}

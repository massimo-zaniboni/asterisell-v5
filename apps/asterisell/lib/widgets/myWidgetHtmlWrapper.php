<?php

/**
 * Insert already rendered HTML content, retrieved from `default` attribute.
 */
class myWidgetHtmlWrapper extends sfWidgetForm
{
    /**
     * Constructor.
     *
     * Available options:
     *
     *  * type: The widget type
     *
     * @param array $options     An array of options
     * @param array $attributes  An array of default HTML attributes
     *
     * @see sfWidgetForm
     */
    protected function configure($options = array(), $attributes = array())
    {
    }

    protected $content;

    public function setContent($v) {
        $this->content = $v;
    }

    public function getContent() {
        return $this->content;
    }

    /**
     * Renders the widget.
     *
     * @param  string $name        The element name
     * @param  string $value       The value displayed in this widget
     * @param  array  $attributes  An array of HTML attributes to be merged with the default HTML attributes
     * @param  array  $errors      An array of errors for the field
     *
     * @return string An HTML tag string
     *
     * @see sfWidgetForm
     */
    public function render($name, $value = null, $attributes = array(), $errors = array())
    {
        return $this->getContent();
    }
}
<?php

/**
 * Inheritance Runtime Methods processBlock, endChild, init
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Inheritance
{

    /**
     * State machine
     * - 0 idle next extends will create a new inheritance tree
     * - 1 processing child template
     * - 2 wait for next inheritance template
     * - 3 assume parent template, if child will loaded goto state 1
     *     a call to a sub template resets the state to 0
     *
     * @var int
     */
    public $state = 0;

    /**
     * Array of root child {block} objects
     *
     * @var Smarty_Internal_Block[]
     */
    public $childRoot = array();

    /**
     * inheritance template nesting level
     *
     * @var int
     */
    public $inheritanceLevel = 0;

    /**
     * inheritance template index
     *
     * @var int
     */
    public $tplIndex = - 1;

    /**
     * current block nesting level
     *
     * @var int
     */
    public $blockNesting = 0;

    /**
     * Array of source template names
     * - key template index
     *
     * @var string[]
     */
    public $templateResource = array();

    /**
     * Initialize inheritance
     *
     * @param \Smarty_Internal_Template $tpl        template object of caller
     * @param bool                      $initChild  if true init for child template
     * @param array                     $blockNames outer level block name
     *
     */
    public function init(Smarty_Internal_Template $tpl, $initChild, $blockNames = array())
    {
        // if called while executing parent template it must be a sub-template with new inheritance root
        if ($initChild && $this->state == 3 && (strpos($tpl->template_resource, 'extendsall') === false)) {
            $tpl->ext->_inheritance = new Smarty_Internal_Runtime_Inheritance();
            $tpl->ext->_inheritance->init($tpl, $initChild, $blockNames);
            return;
        }
        // start of child sub template(s)
        if ($initChild) {
            $this->state = 1;
            if (!$this->inheritanceLevel) {
                //grab any output of child templates
                ob_start();
            }
            $this->inheritanceLevel ++;
        }
        // in parent state {include} will not increment template index
        if ($this->state != 3) {
            $this->tplIndex ++;
            $this->templateResource[ $this->tplIndex ] = $tpl->template_resource;
        }
        // if state was waiting for parent change state to parent
        if ($this->state == 2) {
            $this->state = 3;
        }
    }

    /**
     * End of child template(s)
     * - if outer level is reached flush output buffer and switch to wait for parent template state
     *
     * @param \Smarty_Internal_Template $tpl template object of caller
     */
    public function endChild(Smarty_Internal_Template $tpl)
    {
        $this->inheritanceLevel --;
        if (!$this->inheritanceLevel) {
            ob_end_clean();
            $this->state = 2;
        }
    }
}

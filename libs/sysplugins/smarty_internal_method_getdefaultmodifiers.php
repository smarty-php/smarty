<?php

/**
 * Smarty Method GetDefaultModifiers
 *
 * Smarty::getDefaultModifiers() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
// PHP 8.2+: Allow dynamic properties for method state and data storage
#[\AllowDynamicProperties]
class Smarty_Internal_Method_GetDefaultModifiers
{
    /**
     * Valid for Smarty and template object
     *
     * @var int
     */
    public $objMap = 3;

    /**
     * Get default modifiers
     *
     * @api Smarty::getDefaultModifiers()
     *
     * @param \Smarty_Internal_TemplateBase|\Smarty_Internal_Template|\Smarty $obj
     *
     * @return array list of default modifiers
     */
    public function getDefaultModifiers(Smarty_Internal_TemplateBase $obj)
    {
        $smarty = $obj->_getSmartyObj();
        return $smarty->default_modifiers;
    }
}

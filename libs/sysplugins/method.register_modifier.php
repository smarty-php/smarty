<?php

/**
* Smarty method Register_Modifier
* 
* Registers a PHP function as Smarty modifier plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Register_Modifier
* 
* Register a PHP function as Smarty modifier plugin
*/

class Smarty_Method_Register_Modifier extends Smarty_Internal_Base {
    /**
    * Registers modifier to be used in templates
    * 
    * @param string $modifier name of template modifier
    * @param string $modifier_impl name of PHP function to register
    */
    public function execute($modifier, $modifier_impl)
    {
        if (isset($this->smarty->registered_plugins[$modifier])) {
            throw new Exception("Plugin \"{$modifier}\" already registered");
        } elseif (!is_callable($modifier_impl)) {
            throw new Exception("Plugin \"{$modifier}\" not callable");
        } else {
            $this->smarty->registered_plugins[$modifier] =
            array('modifier', $modifier_impl);
        } 
    } 
} 

?>

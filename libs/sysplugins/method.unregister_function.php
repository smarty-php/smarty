<?php

/**
* Smarty method Unregister_Function
* 
* Unregister a Smarty function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Unregister_Function
* 
* Unregister a Smarty function plugin
*/

class Smarty_Method_Unregister_Function extends Smarty_Internal_Base {
    /**
    * Unregisters custom function
    * 
    * @param string $function_tag name of template function
    */
    public function execute($function_tag)
    {
        if (isset($this->smarty->registered_plugins[$function_tag]) && $this->smarty->registered_plugins[$function_tag][0] == 'function') {
            unset($this->smarty->registered_plugins[$function_tag]);
        } 
    } 
} 
?>

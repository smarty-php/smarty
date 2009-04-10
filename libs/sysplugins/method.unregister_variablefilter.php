<?php

/**
* Smarty method Unregister_Variablefilter
* 
* Unregister a variablefilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Unregister_Variablefilter
* 
* Unregister a variablefilter
*/

class Smarty_Method_Unregister_Variablefilter extends Smarty_Internal_Base {
    /**
    * Unregisters a variablefilter function
    * 
    * @param callback $function 
    */
    public function execute($function)
    {
        $_name = (is_array($function)) ? $function[0] : $function;
        unset($this->smarty->registered_filters['variable'][$_name]);
    } 
} 

?>

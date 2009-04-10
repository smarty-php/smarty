<?php

/**
* Smarty method Register_Variablefilter
* 
* Registers a PHP function as an output filter for variables
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Register_Variablefilter
* 
* Register a PHP function as variablefilter
*/

class Smarty_Method_Register_Variablefilter extends Smarty_Internal_Base {
    /**
    * Registers an output filter function which
    * runs over any variable output
    * 
    * @param callback $function 
    */
    public function execute($function)
    {
        $_name = (is_array($function)) ? $function[0] : $function;
        $this->smarty->registered_filters['variable'][$_name] = $function;
    } 
} 

?>

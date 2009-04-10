<?php

/**
* Smarty method getVariableFilter
* 
* get status of filter on variable output
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getVariableFilter
* 
* get status of filter on variable output
*/
class Smarty_Method_getVariableFilter extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->variable_filter;
    } 
} 

?>

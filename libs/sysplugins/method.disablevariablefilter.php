<?php

/**
* Smarty method disableVariableFilter
* 
* Disable  filter on variable output
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableVariableFilter
* 
* Disable filter on variable output
*/
class Smarty_Method_disableVariableFilter extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->variable_filter = false;
        return;
    } 
} 

?>

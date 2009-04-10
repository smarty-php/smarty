<?php

/**
* Smarty method enableVariableFilter
* 
* Enable  filter on variable output
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableVariableFilter
* 
* Enable filter on variable output
*/
class Smarty_Method_enableVariableFilter extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->variable_filter = true;
        return;
    } 
} 

?>

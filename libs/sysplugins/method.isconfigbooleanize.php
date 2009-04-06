<?php

/**
* Smarty method isConfigBooleanize
* 
* is config booleanize mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isConfigBooleanize
* 
* is config booleanize mode
*/
class Smarty_Method_isConfigBooleanize extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->config_booleanize;
    } 
} 

?>

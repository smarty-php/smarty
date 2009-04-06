<?php

/**
* Smarty method disableConfigBooleanize
* 
* Disable config booleanize mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableConfigBooleanize
* 
* Disable config booleanize mode
*/
class Smarty_Method_disableConfigBooleanize extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->config_booleanize = false;
        return;
    } 
} 

?>

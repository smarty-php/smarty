<?php

/**
* Smarty method enableConfigBooleanize
* 
* Enable config booleanize mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableConfigBooleanize
* 
* Enable config booleanize mode
*/
class Smarty_Method_enableConfigBooleanize extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->config_booleanize = true;
        return;
    } 
} 

?>

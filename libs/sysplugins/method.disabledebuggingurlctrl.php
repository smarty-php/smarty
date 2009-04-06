<?php

/**
* Smarty method disableDebuggingUrlCtrl
* 
* Disable possibility to Disable debugging by SMARTY_DEBUG attribute
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableDebuggingUrlCtrl
* 
* Disable possibility to Disable debugging by SMARTY_DEBUG attribute
*/
class Smarty_Method_disableDebuggingUrlCtrl extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->debugging_ctrl = 'none';
        return;
    } 
} 

?>

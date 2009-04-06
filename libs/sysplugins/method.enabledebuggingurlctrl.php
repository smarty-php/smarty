<?php

/**
* Smarty method enableDebuggingUrlCtrl
* 
* Enable possibility to enable debugging by SMARTY_DEBUG attribute
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableDebuggingUrlCtrl
* 
* Enable possibility to enable debugging by SMARTY_DEBUG attribute
*/
class Smarty_Method_EnableDebuggingUrlCtrl extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->debugging_ctrl = 'URL';
        return;
    } 
} 

?>

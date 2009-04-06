<?php

/**
* Smarty method isDebuggingUrlCtrl
* 
* is possibility to is debugging by SMARTY_DEBUG attribute
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isDebuggingUrlCtrl
* 
* is possibility to is debugging by SMARTY_DEBUG attribute
*/
class Smarty_Method_isDebuggingUrlCtrl extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->debugging_ctrl != 'none';
    } 
} 

?>

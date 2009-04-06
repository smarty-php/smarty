<?php

/**
* Smarty method disableCompileCheck
* 
* Disable compile checking
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableCompileCheck
* 
* Disable compile checking
*/
class Smarty_Method_DisableCompileCheck extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->compile_check = false;
        return;
    } 
} 

?>

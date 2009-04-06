<?php

/**
* Smarty method disableForceCompile
* 
* Disable forced compiling
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableForceCompile
* 
* Disable forced compiling
*/
class Smarty_Method_DisableForceCompile extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->force_compile = false;
        return;
    } 
} 

?>

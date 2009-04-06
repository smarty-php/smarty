<?php

/**
* Smarty method enableForceCompile
* 
* Enable forced compiling
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableForceCompile
* 
* Enable forced compiling
*/
class Smarty_Method_enableForceCompile extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->force_compile = true;
        return;
    } 
} 

?>

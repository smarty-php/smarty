<?php

/**
* Smarty method isForceCompile
* 
* is forced compiling
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isForceCompile
* 
* is forced compiling
*/
class Smarty_Method_isForceCompile extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->force_compile;
    } 
} 

?>

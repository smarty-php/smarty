<?php

/**
* Smarty method isCompileCheck
* 
* is compile checking
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isCompileCheck
* 
* is compile checking
*/
class Smarty_Method_isCompileCheck extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->compile_check;
    } 
} 

?>

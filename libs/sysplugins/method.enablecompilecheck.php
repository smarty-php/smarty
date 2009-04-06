<?php

/**
* Smarty method enableCompileCheck
* 
* Enable compile checking
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableCompileCheck
* 
* Enable compile checking
*/
class Smarty_Method_EnableCompileCheck extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->compile_check = true;
        return;
    } 
} 

?>

<?php

/**
* Smarty method getCompileDir
* 
* Returns directory of compiled templates
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getCompileDir
* 
* Returns directory of compiled templates
*/

class Smarty_Method_GetCompileDir extends Smarty_Internal_Base {
    /**
* Returns directory of compiled templates
    * 
    * @return array compiled template folder
    */
    public function execute()
    {
        return $this->smarty->compile_dir;
    } 
} 

?>

<?php

/**
* Smarty method GetCompileDir
* 
* Returns directory of compiled templates
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Returns directory of compiled templates
*/

/**
* Returns directory of compiled templates
* 
* @return array compiled template folder
*/
function  Smarty_Method_GetCompileDir($smarty)
{
    return $this->smarty->compile_dir;
} 

?>

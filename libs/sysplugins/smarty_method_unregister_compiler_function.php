<?php

/**
* Smarty method Unregister_Compiler_Function
* 
* Unregister a Smarty compiler function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a Smarty compiler function plugin
*/

/**
* Unregisters compiler function
* 
* @param string $compiler_tag name of template function
*/
function  Smarty_Method_Unregister_Compiler_Function($smarty, $compiler_tag)
{
    if (isset($smarty->registered_plugins['compiler'][$compiler_tag])) {
        unset($smarty->registered_plugins['compiler'][$compiler_tag]);
    } 
} 

?>

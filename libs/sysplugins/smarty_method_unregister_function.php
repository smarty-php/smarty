<?php

/**
* Smarty method Unregister_Function
* 
* Unregister a Smarty function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a Smarty function plugin
*/

/**
* Unregisters custom function
* 
* @param string $function_tag name of template function
*/
function  Smarty_Method_Unregister_Function($smarty, $function_tag)
{
    if (isset($smarty->registered_plugins['function'][$function_tag])) {
        unset($smarty->registered_plugins['function'][$function_tag]);
    } 
} 

?>

<?php

/**
* Smarty method Unregister_Variablefilter
* 
* Unregister a variablefilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a variablefilter
*/

/**
* Unregisters a variablefilter function
* 
* @param callback $function 
*/
function unregister_variablefilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    unset($smarty->registered_filters['variable'][$_name]);
} 

?>

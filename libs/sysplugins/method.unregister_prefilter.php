<?php

/**
* Smarty method Unregister_Prefilter
* 
* Unregister a prefilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a prefilter
*/

/**
* Unregisters a prefilter function
* 
* @param callback $function 
*/
function unregister_prefilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    unset($smarty->registered_filters['pre'][$_name]);
} 

?>

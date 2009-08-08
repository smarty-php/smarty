<?php

/**
* Smarty method Register_Variablefilter
* 
* Registers a PHP function as an output filter for variables
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers an output filter function which
* runs over any variable output
* 
* @param object $smarty 
* @param callback $function 
*/
function register_variablefilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    $smarty->registered_filters['variable'][$_name] = $function;
} 

?>

<?php

/**
* Smarty method Register_Prefilter
* 
* Registers a PHP function as prefilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers a prefilter function to apply
* to a template before compiling
* 
* @param object $smarty 
* @param callback $function 
*/
function register_prefilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    $smarty->registered_filters['pre'][$_name] = $function;
} 

?>

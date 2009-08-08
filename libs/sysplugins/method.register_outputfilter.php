<?php

/**
* Smarty method Register_Outputfilter
* 
* Registers a PHP function as outputfilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers an output filter function to apply
* to a template output
* 
* @param object $smarty 
* @param callback $function 
*/
function register_outputfilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    $smarty->registered_filters['output'][$_name] = $function;
} 

?>

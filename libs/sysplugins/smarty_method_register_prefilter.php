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
    $smarty->registered_filters['pre'][$smarty->_get_filter_name($function)] = $function;
} 

?>

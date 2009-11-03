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
function  Smarty_Method_Register_Prefilter($smarty, $function)
{
    $smarty->registered_filters['pre'][$smarty->_get_filter_name($function)] = $function;
} 

?>

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
function  Smarty_Method_Register_Variablefilter($smarty, $function)
{
    $smarty->registered_filters['variable'][$smarty->_get_filter_name($function)] = $function;
} 

?>

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
function  Smarty_Method_Register_Outputfilter($smarty, $function)
{
    $smarty->registered_filters['output'][$smarty->_get_filter_name($function)] = $function;
} 

?>

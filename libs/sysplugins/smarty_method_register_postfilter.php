<?php

/**
* Smarty method Register_Postfilter
* 
* Registers a PHP function as postfilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers a postfilter function to apply
* to a compiled template after compilation
* 
* @param object $smarty 
* @param callback $function 
*/
function  Smarty_Method_Register_Postfilter($smarty, $function)
{
    $smarty->registered_filters['post'][$smarty->_get_filter_name($function)] = $function;
} 

?>

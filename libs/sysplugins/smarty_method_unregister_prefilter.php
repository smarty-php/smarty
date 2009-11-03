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
function  Smarty_Method_Unregister_Prefilter($smarty, $function)
{
    unset($smarty->registered_filters['pre'][$smarty->_get_filter_name($function)]);
} 

?>

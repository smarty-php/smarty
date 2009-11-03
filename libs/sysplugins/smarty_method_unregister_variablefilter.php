<?php

/**
* Smarty method Unregister_Variablefilter
* 
* Unregister a variablefilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a variablefilter
*/

/**
* Unregisters a variablefilter function
* 
* @param callback $function 
*/
function  Smarty_Method_Unregister_Variablefilter($smarty, $function)
{
    unset($smarty->registered_filters['variable'][$smarty->_get_filter_name($function)]);
} 

?>

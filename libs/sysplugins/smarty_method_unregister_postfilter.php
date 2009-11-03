<?php

/**
* Smarty method Unregister_Postfilter
* 
* Unregister a postfilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a postfilter
*/

/**
* Unregisters a postfilter function
* 
* @param callback $function 
*/
function  Smarty_Method_Unregister_Postfilter($smarty, $function)
{
    unset($smarty->registered_filters['post'][$smarty->_get_filter_name($function)]);
} 

?>

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
function unregister_postfilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    unset($smarty->registered_filters['post'][$_name]);
} 

?>

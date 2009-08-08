<?php

/**
* Smarty method Unregister_Outputfilter
* 
* Unregister a outputfilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a outputfilter
*/

/**
* Registers an output filter function to apply
* to a template output
* 
* @param callback $function 
*/
function unregister_outputfilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    unset($smarty->registered_filters['output'][$_name]);
} 

?>

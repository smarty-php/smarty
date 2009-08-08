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
function register_postfilter($smarty, $function)
{
    $_name = (is_array($function)) ? $function[0] : $function;
    $smarty->registered_filters['post'][$_name] = $function;
} 

?>

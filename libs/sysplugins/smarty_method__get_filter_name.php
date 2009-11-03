<?php

/**
* Smarty method _get_filter_name
* 
* Return internal filter name
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Return internal filter name
* 
* @param object $smarty 
* @param callback $function 
*/
function Smarty_Method__get_filter_name($smarty, $function)
{
		if (is_array($function)) {
			$_class_name = (is_object($function[0]) ?
				get_class($function[0]) : $function[0]);
			return $_class_name . '_' . $function[1];
		}
		else {
			return $function;
		}
} 

?>

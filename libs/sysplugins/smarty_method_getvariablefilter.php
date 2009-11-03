<?php

/**
* Smarty method GetVariableFilter
* 
* get status of filter on variable output
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getVariableFilter
* 
* get status of filter on variable output
*/
function  Smarty_Method_GetVariableFilter($smarty)
{
    return $smarty->variable_filter;
} 

?>

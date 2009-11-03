<?php

/**
* Smarty method EnableVariableFilter
* 
* Enable  filter on variable output
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable filter on variable output
*/
function  Smarty_Method_EnableVariableFilter($smarty)
{
    $smarty->variable_filter = true;
    return;
} 

?>

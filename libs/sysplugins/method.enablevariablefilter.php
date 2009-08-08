<?php

/**
* Smarty method enableVariableFilter
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
function enableVariableFilter($smarty)
{
    $smarty->variable_filter = true;
    return;
} 

?>

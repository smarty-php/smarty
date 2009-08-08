<?php

/**
* Smarty method disableVariableFilter
* 
* Disable  filter on variable output
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable filter on variable output
*/
function disableVariableFilter($smarty)
{
    $smarty->variable_filter = false;
    return;
} 

?>

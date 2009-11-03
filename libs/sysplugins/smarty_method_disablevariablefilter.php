<?php

/**
* Smarty method DisableVariableFilter
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
function  Smarty_Method_DisableVariableFilter($smarty)
{
    $smarty->variable_filter = false;
    return;
} 

?>

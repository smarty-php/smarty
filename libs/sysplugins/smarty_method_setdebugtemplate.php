<?php

/**
* Smarty method SetDebugTemplate
* 
* Sets debug template filepath
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Sets debug template filepath
*/

/**
* Sets debug template filepath
* 
* @param string $ array debug template filepath
*/
function  Smarty_Method_SetDebugTemplate($smarty, $debug_tpl)
{
    $smarty->debug_tpl = $debug_tpl;
    return;
} 

?>

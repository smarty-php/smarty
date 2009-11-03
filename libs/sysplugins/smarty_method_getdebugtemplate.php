<?php

/**
* Smarty method GetDebugTemplate
* 
* Returns debug template filepath
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Returns debug template filepath
*/

/**
* Returns directory of cache files
* 
* @return string debug template filepath
*/
function  Smarty_Method_GetDebugTemplate($smarty)
{
    return $smarty->debug_tpl;
} 

?>

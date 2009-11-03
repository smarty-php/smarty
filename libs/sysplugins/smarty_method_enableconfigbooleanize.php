<?php

/**
* Smarty method EnableConfigBooleanize
* 
* Enable config booleanize mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable config booleanize mode
*/
function  Smarty_Method_EnableConfigBooleanize($smarty)
{
    $smarty->config_booleanize = true;
    return;
} 

?>

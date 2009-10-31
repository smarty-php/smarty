<?php

/**
* Smarty method enableConfigBooleanize
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
function enableConfigBooleanize($smarty)
{
    $smarty->config_booleanize = true;
    return;
} 

?>

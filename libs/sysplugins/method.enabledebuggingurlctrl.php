<?php

/**
* Smarty method enableDebuggingUrlCtrl
* 
* Enable possibility to enable debugging by SMARTY_DEBUG attribute
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Enable possibility to enable debugging by SMARTY_DEBUG attribute
*/
function enableDebuggingUrlCtrl($smarty)
{
    $smarty->debugging_ctrl = 'URL';
    return;
} 

?>

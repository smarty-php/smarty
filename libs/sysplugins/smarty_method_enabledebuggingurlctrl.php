<?php

/**
* Smarty method EnableDebuggingUrlCtrl
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
function  Smarty_Method_EnableDebuggingUrlCtrl($smarty)
{
    $smarty->debugging_ctrl = 'URL';
    return;
} 

?>

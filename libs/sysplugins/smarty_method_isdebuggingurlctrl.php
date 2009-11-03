<?php

/**
* Smarty method IsDebuggingUrlCtrl
* 
* is possibility to is debugging by SMARTY_DEBUG attribute
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* is possibility to is debugging by SMARTY_DEBUG attribute
*/
function  Smarty_Method_IsDebuggingUrlCtrl($smarty)
{
    return $smarty->debugging_ctrl != 'none';
} 

?>

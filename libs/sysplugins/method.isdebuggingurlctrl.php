<?php

/**
* Smarty method isDebuggingUrlCtrl
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
function isDebuggingUrlCtrl($smarty)
{
    return $smarty->debugging_ctrl != 'none';
} 

?>

<?php

/**
* Smarty method DisableDebuggingUrlCtrl
* 
* Disable possibility to Disable debugging by SMARTY_DEBUG attribute
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable possibility to Disable debugging by SMARTY_DEBUG attribute
*/
function  Smarty_Method_DisableDebuggingUrlCtrl($smarty)
    {
        $smarty->debugging_ctrl = 'none';
        return;
    } 

?>

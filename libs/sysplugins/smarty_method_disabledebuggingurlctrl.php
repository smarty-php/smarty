<?php

/**
* Smarty method disableDebuggingUrlCtrl
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
function disableDebuggingUrlCtrl($smarty)
    {
        $smarty->debugging_ctrl = 'none';
        return;
    } 

?>

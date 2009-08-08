<?php

/**
* Smarty method disableConfigBooleanize
* 
* Disable config booleanize mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable config booleanize mode
*/
function disableConfigBooleanize($smarty)
{
    $this->smarty->config_booleanize = false;
    return;
} 

?>

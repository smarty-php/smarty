<?php

/**
* Smarty method DisableConfigBooleanize
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
function  Smarty_Method_DisableConfigBooleanize($smarty)
{
    $this->smarty->config_booleanize = false;
    return;
} 

?>

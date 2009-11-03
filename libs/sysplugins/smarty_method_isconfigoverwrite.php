<?php

/**
* Smarty method IsConfigOverwrite
* 
* is config overwrite mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* is config overwrite mode
*/
function  Smarty_Method_IsConfigOverwrite($smarty)
{
    return $smarty->config_overwrite;
} 

?>

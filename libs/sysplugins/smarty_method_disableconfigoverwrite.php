<?php

/**
* Smarty method DisableConfigOverwrite
* 
* Disable config overwrite mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Disable config overwrite mode
*/
function  Smarty_Method_DisableConfigOverwrite($smarty)
{
    $smarty->config_overwrite = false;
    return ;
} 
?>

<?php

/**
* Smarty method disableConfigOverwrite
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
function disableConfigOverwrite($smarty)
{
    $smarty->config_overwrite = false;
    return ;
} 
?>

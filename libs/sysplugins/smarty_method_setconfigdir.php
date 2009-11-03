<?php

/**
* Smarty method SetConfigDir
* 
* Sets directory of config files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Sets directory of config files
* 
* @param object $smarty 
* @param string $ config folder
* @return 
*/
function  Smarty_Method_SetConfigDir($smarty, $config_dir)
{
    $this->smarty->config_dir = $config_dir;
    return;
} 

?>

<?php

/**
* Smarty method addPluginsDir
* 
* Adds directory of plugin files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Adds directory of plugin files
* 
* @param object $smarty 
* @param string $ |array $ plugins folder
* @return 
*/
function  Smarty_Method_AddPluginsDir($smarty, $plugins_dir)
{
    $smarty->plugins_dir = array_merge((array)$smarty->plugins_dir, (array)$plugins_dir);
    $smarty->plugins_dir = array_unique($smarty->plugins_dir);
    return;
} 

?>

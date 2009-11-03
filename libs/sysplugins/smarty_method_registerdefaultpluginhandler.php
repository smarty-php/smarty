<?php

/**
* Smarty method RegisterDefaultPluginhandlerHandler
* 
* Registers a default plugin handler
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers a default plugin handler
* 
* @param object $smarty 
* @param string $ |array $plugin class/methode name
*/
function  Smarty_Method_RegisterDefaultPluginHandler($smarty, $plugin)
{
    if (is_callable($plugin)) {
        $smarty->default_plugin_handler_func = $plugin;
    } else {
        throw new Exception('Default plugin handler "' . $plugin . '" not callable');
    } 
} 

?>

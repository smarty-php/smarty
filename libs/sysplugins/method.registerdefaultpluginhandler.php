<?php

/**
* Smarty method registerDefaultPluginhandlerHandler
* 
* Registers a default plugin handler
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class registerDefaultPluginHandler
* 
* Registers a default plugin handler
*/

class Smarty_Method_registerDefaultPluginHandler extends Smarty_Internal_Base {
    /**
* Registers a default plugin handler
    * 
    * @param string $ |array $plugin class/methode name
    */
    public function execute($plugin)
    {
        if (is_callable($plugin)) {
            $this->smarty->default_plugin_handler_func = $plugin;
        } else {
            throw new Exception('Default plugin handler "'.$plugin.'" not callable');
        } 
    } 
} 

?>

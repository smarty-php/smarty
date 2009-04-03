<?php

/**
* Smarty Internal Plugin Handler
* 
* @package Smarty
* @subpackage PluginsInternal
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Handler Class
*/
class Smarty_Internal_Plugin_Handler extends Smarty_Internal_Base {
    /**
    * Call a Smarty plugin
    * 
    * @param string $name block function name
    * @param array $args $args[0] = array of plugin attributes and $args[1] = plugin types to search for
    */
    public function __call($name, $args)
    {
        if ($this->loadSmartyPlugin($name, $args[1])) {
            // call plugin
            return call_user_func_array($this->smarty->registered_plugins[$name][1], $args[0]);
        } else {
            // plugin not found
            throw new Exception("Unable to load plugin {$name}");
        } 
    } 
    /**
    * Lazy loads plugin files
    * class name format: Smarty_PluginType_FuncName
    * plugin filename format: plugintype.funcname.php
    * 
    * @param string $name plugin name
    * @param array $type array of plugin types to search for
    */
    public function loadSmartyPlugin($name, $type)
    { 
        // load plugin if missing
        if (isset($this->smarty->registered_plugins[$name])) {
            return true;
        } else {
            foreach ((array)$type as $plugin_type) {
                $plugin = 'smarty_' . $plugin_type . '_' . $name;
                if ($this->smarty->loadPlugin($plugin)) {
                    if (class_exists($plugin, false)) {
                        $plugin = array($plugin, 'execute');
                    } 
                    if (is_callable($plugin)) {
                        $this->smarty->registered_plugins[$name] = array($plugin_type, $plugin, false);
                        return true;
                    } else {
                        throw new Exception("Plugin \"{$name}\" not callable");
                    } 
                } 
            } 
        } 
        if (!empty($this->smarty->default_plugin_handler_func)) {
            if (!is_callable($this->smarty->default_plugin_handler_func)) {
                throw new Exception("Default template handler not callable");
            } else {
                return call_user_func_array($this->smarty->default_plugin_handler_func, array($name, $type, &$this));
            } 
        } 
        return false;
    } 
} 

?>

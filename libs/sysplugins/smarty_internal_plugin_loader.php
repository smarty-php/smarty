<?php
/**
 * Smarty Internal Plugin Loader
 *
 * Compiles the {ldelim} tag 
 * @package Smarty
 * @subpackage PluginsInternal
 * @author Uwe Tews
 */

/**
 * Smarty Internal Plugin Loader Class
 */ 
class Smarty_Internal_Plugin_Loader {
    /**
     * Takes unknown classes and loads plugin files for them
     * class name format: Smarty_PluginType_PluginName
     * plugin filename format: plugintype.pluginname.php
     * 
     * @param string $plugin_name class plugin name to load
     * @return string |boolean filepath of loaded file or false
     */
    static function loadPlugin($plugin_name, $plugins_dir)
    { 
        // if function or class exists, exit silently (already loaded)
        if (is_callable($plugin_name) || class_exists($plugin_name, false))
            return true; 
        // Plugin name is expected to be: Smarty_[Type]_[Name]
        $_plugin_name = strtolower($plugin_name);
        $_name_parts = explode('_', $_plugin_name, 3); 
        // class name must have three parts to be valid plugin
        if (count($_name_parts) < 3 || $_name_parts[0] !== 'smarty') {
            throw new SmartyException("plugin {$plugin_name} is not a valid name format");
            return false;
        } 
        // if type is "internal", get plugin from sysplugins
        if ($_name_parts[1] == 'internal') {
            $file = SMARTY_SYSPLUGINS_DIR . $_plugin_name . '.php';
            if (file_exists($file)) {
                require_once($file);
                return $file;
            } else {
                return false;
            } 
        } 
        // plugin filename is expected to be: [type].[name].php
        $_plugin_filename = "{$_name_parts[1]}.{$_name_parts[2]}.php"; 
        // loop through plugin dirs and find the plugin
        foreach((array)$plugins_dir as $_plugin_dir) {
            if (strpos('/\\', substr($_plugin_dir, -1)) === false) {
                $_plugin_dir .= DS;
            } 
            $file = $_plugin_dir . $_plugin_filename;
            if (file_exists($file)) {
                require_once($file);
                return $file;
            } 
        } 
        // no plugin loaded
        return false;
    } 
}

?>

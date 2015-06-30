<?php

/**
 * Smarty Extension Loadplugin
 *
 * $smarty->loadPlugin() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_LoadPlugin
{
    /**
     * Takes unknown classes and loads plugin files for them
     * class name format: Smarty_PluginType_PluginName
     * plugin filename format: plugintype.pluginname.php
     *
     * @param \Smarty $smarty
     * @param  string $plugin_name class plugin name to load
     * @param  bool   $check       check if already loaded
     *
     * @return bool|string
     * @throws \SmartyException
     */
    public static function loadPlugin(Smarty $smarty, $plugin_name, $check)
    {
        // if function or class exists, exit silently (already loaded)
        if ($check && (is_callable($plugin_name) || class_exists($plugin_name, false))) {
            return true;
        }
        if (!preg_match('#^smarty_((internal)|([^_]+))_(.+)$#i', $plugin_name, $match)) {
            throw new SmartyException("plugin {$plugin_name} is not a valid name format");
        }
        if (!empty($match[2])) {
            $file = SMARTY_SYSPLUGINS_DIR . strtolower($plugin_name) . '.php';
            if (isset($smarty->_is_file_cache[$file])) {
                if ($smarty->_is_file_cache[$file] !== false) {
                    return $smarty->_is_file_cache[$file];
                } else {
                    return false;
                }
            } else {
                if (is_file($file)) {
                    $smarty->_is_file_cache[$file] = $file;
                    require_once($file);
                    return $file;
                } else {
                    $smarty->_is_file_cache[$file] = false;
                    return false;
                }
            }
        }
        // plugin filename is expected to be: [type].[name].php
        $_plugin_filename = "{$match[1]}.{$match[4]}.php";
        $_lower_filename = strtolower($_plugin_filename);
        $_different = $_lower_filename != $_plugin_filename;
        // loop through plugin dirs and find the plugin
        $names = array();
        foreach ($smarty->getPluginsDir() as $_plugin_dir) {
            $names[] = $_plugin_dir . $_plugin_filename;
            if ($_different) {
                $names[] = $_plugin_dir . $_lower_filename;
            }
        }
        foreach ($names as $path) {
            $file = $smarty->use_include_path ? $smarty->_realpath($path) : $path;
            if (isset($smarty->_is_file_cache[$file])) {
                if ($smarty->_is_file_cache[$file] !== false) {
                    return $smarty->_is_file_cache[$file];
                }
            }
            if (is_file($file)) {
                $smarty->_is_file_cache[$file] = $file;
                require_once($file);
                return $file;
            }
            $smarty->_is_file_cache[$file] = false;
        }
        if ($smarty->use_include_path) {
            // try PHP include_path
            $path = Smarty_Internal_Get_Include_Path::getIncludePath($names, null, $smarty);
            if ($path !== false) {
                $smarty->_is_file_cache[$path] = $path;
                require_once($path);
                return $path;
            }
        }
        // no plugin loaded
        return false;
    }
}
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
            if (isset($smarty->_cache['plugin_files'][$file])) {
                if ($smarty->_cache['plugin_files'][$file] !== false) {
                    return $smarty->_cache['plugin_files'][$file];
                } else {
                    return false;
                }
            } else {
                if (is_file($file)) {
                    $smarty->_cache['plugin_files'][$file] = $file;
                    require_once($file);
                    return $file;
                } else {
                    $smarty->_cache['plugin_files'][$file] = false;
                    return false;
                }
            }
        }
        // plugin filename is expected to be: [type].[name].php
        $_plugin_filename = "{$match[1]}.{$match[4]}.php";
        $_lower_filename = strtolower($_plugin_filename);
        if (isset($smarty->_cache['plugin_files'])) {
            if (isset($smarty->_cache['plugin_files']['plugins_dir'][$_lower_filename])) {
                if (!$smarty->use_include_path ||
                    $smarty->_cache['plugin_files']['plugins_dir'][$_lower_filename] !== false
                ) {
                    return $smarty->_cache['plugin_files']['plugins_dir'][$_lower_filename];
                }
            }
            if (!$smarty->use_include_path || Smarty_Internal_Get_Include_Path::isNewIncludePath($smarty)) {
                unset($smarty->_cache['plugin_files']['include_path']);
            } else {
                if (isset($smarty->_cache['plugin_files']['include_path'][$_lower_filename])) {
                    return $smarty->_cache['plugin_files']['include_path'][$_lower_filename];
                }
            }
        }
        $_file_names = array($_plugin_filename);
        if ($_lower_filename != $_plugin_filename) {
            $_file_names[] = $_lower_filename;
        }
        $_p_dirs = $smarty->getPluginsDir();
        if (!isset($smarty->_cache['plugin_files']['plugins_dir'][$_lower_filename])) {
            // loop through plugin dirs and find the plugin
            foreach ($_p_dirs as $_plugin_dir) {
                foreach ($_file_names as $name) {
                    $file = $_plugin_dir . $name;
                    if (is_file($file)) {
                        $smarty->_cache['plugin_files']['plugins_dir'][$_lower_filename] = $file;
                        require_once($file);
                        return $file;
                    }
                    $smarty->_cache['plugin_files']['plugins_dir'][$_lower_filename] = false;
                }
            }
        }
        if ($smarty->use_include_path) {
            foreach ($_file_names as $_file_name) {
                // try PHP include_path
                $file = Smarty_Internal_Get_Include_Path::getIncludePath($_p_dirs, $_file_name, $smarty);
                $smarty->_cache['plugin_files']['include_path'][$_lower_filename] = $file;
                if ($file !== false) {
                    require_once($file);
                    return $file;
                }
            }
        }
        // no plugin loaded
        return false;
    }
}
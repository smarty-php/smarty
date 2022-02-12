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
class Smarty_Internal_Method_LoadPlugin
{
    /**
     * Cache of searched plugin files
     *
     * @var array
     */
    public $plugin_files = array();

    /**
     * Takes unknown classes and loads plugin files for them
     * class name format: Smarty_PluginType_PluginName
     * plugin filename format: plugintype.pluginname.php
     *
     * plugin classes may also be autoloaded and will be
     * used, if they implement the appropriate method
     *
     * @param \Smarty $smarty
     * @param string  $plugin_name class plugin name to load
     * @param bool    $check       check if already loaded
     *
     * @return bool|string|array
     * @throws \SmartyException
     */
    public function loadPlugin(Smarty $smarty, $plugin_prefix, $plugin_type, $name, $check)
    {
        // if function or class exists, exit silently (already loaded)
	$plugin_name = "{$plugin_prefix}_{$plugin_type}_{$name}";
        if ($check && (is_callable($plugin_name) || class_exists($plugin_name, false))) {
            return true;
        }

        // check for auto-loader plugin
        foreach ($smarty->getPluginsNamespace() as $namespace) {
            $class = $namespace . '\\' . $name;
            if (class_exists($class, true) && is_callable(array($class, $plugin_type))) {
                // the requested class exists and implements the required interface.
                // as we are using the autoloader, we do not know the file name
                if ($check) return true;
                else return array($class, $plugin_type);
            }
        }

        if (!preg_match('#^smarty_((internal)|([^_]+))_(.+)$#i', $plugin_name, $match)) {
            throw new SmartyException("plugin {$plugin_name} is not a valid name format");
        }
        if (!empty($match[ 2 ])) {
            $file = SMARTY_SYSPLUGINS_DIR . strtolower($plugin_name) . '.php';
            if (isset($this->plugin_files[ $file ])) {
                if ($this->plugin_files[ $file ] !== false) {
                    return $this->plugin_files[ $file ];
                } else {
                    return false;
                }
            } else {
                if (is_file($file)) {
                    $this->plugin_files[ $file ] = $file;
                    include_once $file;
                    return $file;
                } else {
                    $this->plugin_files[ $file ] = false;
                    return false;
                }
            }
        }
        // plugin filename is expected to be: [type].[name].php
        $_plugin_filename = "{$match[1]}.{$match[4]}.php";
        $_lower_filename = strtolower($_plugin_filename);
        if (isset($this->plugin_files)) {
            if (isset($this->plugin_files[ 'plugins_dir' ][ $_lower_filename ])) {
                if (!$smarty->use_include_path || $this->plugin_files[ 'plugins_dir' ][ $_lower_filename ] !== false) {
                    return $this->plugin_files[ 'plugins_dir' ][ $_lower_filename ];
                }
            }
            if (!$smarty->use_include_path || $smarty->ext->_getIncludePath->isNewIncludePath($smarty)) {
                unset($this->plugin_files[ 'include_path' ]);
            } else {
                if (isset($this->plugin_files[ 'include_path' ][ $_lower_filename ])) {
                    return $this->plugin_files[ 'include_path' ][ $_lower_filename ];
                }
            }
        }
        $_file_names = array($_plugin_filename);
        if ($_lower_filename !== $_plugin_filename) {
            $_file_names[] = $_lower_filename;
        }
        $_p_dirs = $smarty->getPluginsDir();
        if (!isset($this->plugin_files[ 'plugins_dir' ][ $_lower_filename ])) {
            // loop through plugin dirs and find the plugin
            foreach ($_p_dirs as $_plugin_dir) {
                foreach ($_file_names as $name) {
                    $file = $_plugin_dir . $name;
                    if (is_file($file)) {
                        $this->plugin_files[ 'plugins_dir' ][ $_lower_filename ] = $file;
                        include_once $file;
                        return $file;
                    }
                    $this->plugin_files[ 'plugins_dir' ][ $_lower_filename ] = false;
                }
            }
        }
        if ($smarty->use_include_path) {
            foreach ($_file_names as $_file_name) {
                // try PHP include_path
                $file = $smarty->ext->_getIncludePath->getIncludePath($_p_dirs, $_file_name, $smarty);
                $this->plugin_files[ 'include_path' ][ $_lower_filename ] = $file;
                if ($file !== false) {
                    include_once $file;
                    return $file;
                }
            }
        }
        // no plugin loaded
        return false;
    }
}

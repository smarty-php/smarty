<?php

/**
 * Smarty Extension AutoLoadPlugin
 *
 * $smarty->autoLoadPlugin() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Stefan Froehlich
 */
class Smarty_Internal_Method_AutoLoadPlugin
{
    /**
     * Takes unknown classes and tries to load the
     * appropriate plugin file via autoloader.
     * The namespace must be registered with Smarty,
     * the class name must match the plugin name and
     * there needs to be a method named like the plugin
     * type.
     *
     * @param \Smarty $smarty
     * @param string  $plugin_name class plugin name to load
     * @param bool    $check       check if already loaded
     *
     * @return bool|string|array
     * @throws \SmartyException
     */
    public function autoLoadPlugin(Smarty $smarty, String $plugin_name, Bool $check)
    {
    	// Naming convention of plugins is "Smarty_type_name" (corresponds
	// to the file name of procedural plugins). We need to extract type
	// and name
        if (!preg_match('#^smarty_((internal)|([^_]+))_(.+)$#i', $plugin_name, $matches)) {
            throw new SmartyException("plugin {$plugin_name} is not a valid name format");
        }

        // if function or class exists, exit silently (procedural plugin already loaded)
        if ($check && (is_callable($plugin_name) || class_exists($plugin_name, false))) {
            return true;
        }

        // check for auto-loader plugin
        foreach ($smarty->getPluginsNamespaces() as $namespace) {
            $class = $namespace . '\\' . $matches[4];
            if (class_exists($class, true) && is_callable(array($class, $matches[1]))) {
                // the requested class exists and the required method exists
                if ($check) return true;
                else return array($class, $matches[1]);
            }
        }

        // no plugin loaded
        return false;
    }
}

<?php
/**
 * Smarty read include path plugin
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Monte Ohrt
 */

/**
 * Smarty Internal Read Include Path Class
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 */
class Smarty_Internal_Get_Include_Path
{
    /**
     * Return full file path from PHP include_path
     *
     * @param  string[] $dirs
     * @param  string   $file
     * @param \Smarty   $smarty
     *
     * @return bool|string full filepath or false
     *
     */
    public static function getIncludePath($dirs, $file, Smarty $smarty)
    {
        static $_include_path = null;
        static $_has_stream_include = null;
        if ($_include_path === null) {
            $_include_path = (array) explode(PATH_SEPARATOR, get_include_path());
            foreach ($_include_path as $key => $_path) {
                $_include_path[$key] = rtrim($_path, '/\\');
            }
            $_has_stream_include = function_exists('stream_resolve_include_path');
        }
        // try PHP include_path
        foreach ($dirs as $dir) {
            if ($dir[0] != '/' && $dir[1] != ':') {
                $_d_path = $dir . isset($file) ? $file : '';
                if ($_has_stream_include) {
                    // available since PHP 5.3.2
                    $path = stream_resolve_include_path($_d_path);
                    if ($path !== false && is_file($path)) {
                        return $smarty->_realpath($path);
                    }
                }
                foreach ($_include_path as $_i_path) {
                    if (is_file($_i_path . DS . $_d_path)) {
                        return $smarty->_realpath($_i_path . DS . $_d_path);
                    }
                }
            }
        }
        return false;
    }
}

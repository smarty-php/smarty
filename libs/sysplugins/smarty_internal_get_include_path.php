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
     * include path cache
     *
     * @var string
     */
    static $_include_path = '';

    /**
     * include path directory cache
     *
     * @var array
     */
    static $_include_dirs = array();

    /**
     * include path directory cache
     *
     * @var array
     */
    static $_user_dirs = array();

    /**
     * stream cache
     *
     * @var array
     */
    static $stream = array();

    /**
     * stream cache
     *
     * @var array
     */
    static $isFile = array();

    /**
     * stream cache
     *
     * @var array
     */
    static $isPath = array();

    /**
     * stream cache
     *
     * @var array
     */
    static $number = array();

    /**
     * status cache
     *
     * @var null
     */
    static $_has_stream_include = null;

    /**
     * Numger for array index
     *
     * @var int
     */
    static $counter = 0;

    /**
     * Check if include path was updated
     *
     * @return bool
     *
     */
    public static function isNewIncludePath(Smarty $smarty)
    {
        if (!isset(self::$_has_stream_include)) {
            self::$_has_stream_include = ($smarty->use_include_path === 2) &&
                function_exists('stream_resolve_include_path');
        }
        $_i_path = get_include_path();
        if (self::$_include_path != $_i_path) {
            self::$_include_dirs = array();
            self::$_include_path = $_i_path;
            $_dirs = (array) explode(PATH_SEPARATOR, $_i_path);
            foreach ($_dirs as $_path) {
                if ($_path[0] != '/' && isset($dir[1]) && $dir[1] != ':') {
                    $_path = $smarty->_realpath($_path . DS, true);
                }
                if (is_dir($_path)) {
                    self::$_include_dirs[] = $smarty->_realpath($_path . DS, true);
                }
            }
            return true;
        }
        return false;
    }

    /**
     * return array with include path directories
     *
     * @return array
     */
    public static function getIncludePathDirs(Smarty $smarty)
    {
        Smarty_Internal_Get_Include_Path::isNewIncludePath($smarty);
        return self::$_include_dirs;
    }

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
        self::isNewIncludePath($smarty);
        // try PHP include_path
        foreach ($dirs as $dir) {
            $dir_n = isset(self::$number[$dir]) ? self::$number[$dir] : self::$number[$dir] = self::$counter ++;
            if (isset(self::$isFile[$dir_n][$file])) {
                if (self::$isFile[$dir_n][$file]) {
                    return self::$isFile[$dir_n][$file];
                } else {
                    continue;
                }
            }
            if (isset(self::$_user_dirs[$dir_n])) {
                if (false === self::$_user_dirs[$dir_n]) {
                    continue;
                } else {
                    $_u_dir = self::$_user_dirs[$dir_n];
                }
            } else {
                if ($dir[0] == '/' || $dir[1] == ':') {
                    $_u_dir = str_ireplace(getcwd(), '.', $dir);
                    if ($_u_dir[0] == '/' || $_u_dir[1] == ':') {
                        self::$_user_dirs[$dir_n] = false;
                        continue;
                    }
                    self::$_user_dirs[$dir_n] = $_u_dir;
                } else {
                    $_u_dir = self::$_user_dirs[$dir_n] = $dir;
                }
            }
            $_d_path = $_u_dir . (isset($file) ? $file : '');
            if (self::$_has_stream_include) {
                // available since PHP 5.3.2
                self::$stream[$_d_path] = isset(self::$stream[$_d_path]) ? self::$stream[$_d_path] : ($path = stream_resolve_include_path($_d_path)) ? is_file($path) : false;
                if (self::$stream[$_d_path]) {
                    return self::$isFile[$dir_n][$file] = self::$stream[$_d_path];
                }
            }
            foreach (self::$_include_dirs as $key => $_i_path) {
                $path = self::$isPath[$key][$dir_n] = isset(self::$isPath[$key][$dir_n]) ? self::$isPath[$key][$dir_n] : is_dir($_i_path .
                                                                                                                                $_u_dir) ? $_i_path .
                    substr($_u_dir, 2) : false;
                $_file = self::$isFile[$dir_n][$file] = ($path && is_file($path . $file)) ? $path . $file : false;
                if ($_file) {
                    return $_file;
                }
            }
        }
        return false;
    }
}

<?php

/**
 * Smarty Extension Filter
 *
 * Register filter methods
 * Load filter method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_Filter
{
    /**
     * Valid filter types
     *
     * @var array
     */
    static $filterTypes = array('pre' => true, 'post' => true, 'output' => true, 'variable' => true);

    /**
     * Registers a filter function
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $type filter type
     * @param  callback                          $callback
     * @param  string|null                       $name optional filter name
     *
     * @throws \SmartyException
     */
    static function registerFilter($obj, $type, $callback, $name)
    {
        self::_checkFilterType($type);
        $name = isset($name) ? $name : self::_getFilterName($callback);
        if (!is_callable($callback)) {
            throw new SmartyException("{$type}filter \"{$name}\" not callable");
        }
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        $smarty->registered_filters[$type][$name] = $callback;
    }

    /**
     * Unregisters a filter function
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $type filter type
     * @param  callback|string                   $callback
     *
     */
    static function unregisterFilter($obj, $type, $callback)
    {
        self::_checkFilterType($type);
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        if (isset($smarty->registered_filters[$type])) {
            $name = is_string($callback) ? $callback : self::_getFilterName($callback);
            if (isset($smarty->registered_filters[$type][$name])) {
                unset($smarty->registered_filters[$type][$name]);
                if (empty($smarty->registered_filters[$type])) {
                    unset($smarty->registered_filters[$type]);
                }
            }
        }
    }

    /**
     * load a filter of specified type and name
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $type filter type
     * @param  string                            $name filter name
     *
     * @return bool
     * @throws SmartyException if filter could not be loaded
     */
    static function loadFilter($obj, $type, $name)
    {
        self::_checkFilterType($type);
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        $_plugin = "smarty_{$type}filter_{$name}";
        $_filter_name = $_plugin;
        if ($smarty->loadPlugin($_plugin)) {
            if (class_exists($_plugin, false)) {
                $_plugin = array($_plugin, 'execute');
            }
            if (is_callable($_plugin)) {
                $smarty->registered_filters[$type][$_filter_name] = $_plugin;
                return true;
            }
        }
        throw new SmartyException("{$type}filter \"{$name}\" not callable");
    }

    /**
     * unload a filter of specified type and name
     *
     * @param  \Smarty_Internal_Template|\Smarty $obj
     * @param  string                            $type filter type
     * @param  string                            $name filter name
     *
     */
    static function unloadFilter($obj, $type, $name)
    {
        self::_checkFilterType($type);
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        if (isset($smarty->registered_filters[$type])) {
            $_filter_name = "smarty_{$type}filter_{$name}";
            if (isset($smarty->registered_filters[$type][$_filter_name])) {
                unset ($smarty->registered_filters[$type][$_filter_name]);
                if (empty($smarty->registered_filters[$type])) {
                    unset($smarty->registered_filters[$type]);
                }
            }
        }
    }

    /**
     * Return internal filter name
     *
     * @param  callback $function_name
     *
     * @return string   internal filter name
     */
    static function _getFilterName($function_name)
    {
        if (is_array($function_name)) {
            $_class_name = (is_object($function_name[0]) ? get_class($function_name[0]) : $function_name[0]);

            return $_class_name . '_' . $function_name[1];
        } elseif (is_string($function_name)) {
            return $function_name;
        } else {
            return 'closure';
        }
    }

    /**
     * Check if filter type is valid
     *
     * @param string $type
     *
     * @throws \SmartyException
     */
    static function _checkFilterType($type)
    {
        if (!isset(self::$filterTypes[$type])) {
            throw new SmartyException("Illegal filter type \"{$type}\"");
        }
    }
}
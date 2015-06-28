<?php

/**
 * Smarty Extension AutoLoadFilter
 *
 * Auto load filter methods
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_AutoLoadFilter
{
    /**
     * Valid filter types
     *
     * @var array
     */
    static $filterTypes = array('pre' => true, 'post' => true, 'output' => true, 'variable' => true);

    /**
     * Set autoload filters
     *
     * @param \Smarty $smarty
     * @param  array  $filters filters to load automatically
     * @param  string $type    "pre", "output", … specify the filter type to set. Defaults to none treating $filters'
     *                         keys as the appropriate types
     */
    public static function setAutoloadFilters(Smarty $smarty, $filters, $type)
    {
        if ($type !== null) {
            self::_checkFilterType($type);
            $smarty->autoload_filters[$type] = (array) $filters;
        } else {
            foreach ((array) $filters as $type => $value) {
                self::_checkFilterType($type);
            }
            $smarty->autoload_filters = (array) $filters;
        }
    }

    /**
     * Add autoload filters
     *
     * @param \Smarty $smarty
     * @param  array  $filters filters to load automatically
     * @param  string $type    "pre", "output", … specify the filter type to set. Defaults to none treating $filters'
     *                         keys as the appropriate types
     */
    public static function addAutoloadFilters(Smarty $smarty, $filters, $type)
    {
        if ($type !== null) {
            self::_checkFilterType($type);
            if (!empty($smarty->autoload_filters[$type])) {
                $smarty->autoload_filters[$type] = array_merge($smarty->autoload_filters[$type], (array) $filters);
            } else {
                $smarty->autoload_filters[$type] = (array) $filters;
            }
        } else {
            foreach ((array) $filters as $type => $value) {
                self::_checkFilterType($type);
                if (!empty($smarty->autoload_filters[$type])) {
                    $smarty->autoload_filters[$type] = array_merge($smarty->autoload_filters[$type], (array) $value);
                } else {
                    $smarty->autoload_filters[$type] = (array) $value;
                }
            }
        }
    }

    /**
     * Get autoload filters
     *
     * @param \Smarty $smarty
     * @param  string $type type of filter to get auto loads for. Defaults to all autoload filters
     *
     * @return array array( 'type1' => array( 'filter1', 'filter2', … ) ) or array( 'filter1', 'filter2', …) if $type
     *                was specified
     */
    public static function getAutoloadFilters(Smarty $smarty, $type)
    {
        if ($type !== null) {
            self::_checkFilterType($type);
            return isset($smarty->autoload_filters[$type]) ? $smarty->autoload_filters[$type] : array();
        }
        return $smarty->autoload_filters;
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
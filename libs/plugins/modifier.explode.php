<?php
/**
 * Smarty plugin
 *
 * @package    Smarty
 * @subpackage PluginsModifier
 */
/**
 * Smarty explode modifier plugin
 * Type:     modifier
 * Name:     explode
 * Purpose:  php explode wrapper to fix PHP 8.1 "Passing null to parameter #2 ($string) of type string is deprecated"
 *
 * @author Christian Meius <https://github.com/cmelius>
 *
 * @param string $separator string to seperate $string with
 * @param string $string  text to seperate
 * @param string $limit max replacements
 *
 * @return array
 */
function smarty_modifier_explode($separator, $string, $limit = PHP_INT_MAX)
{
    static $is_loaded = false;

    $string = null !== $string ? $string : '';

    return explode($separator, $string, $limit);
}

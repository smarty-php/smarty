<?php
/**
 * Smarty plugin
 *
 * @package    Smarty
 * @subpackage PluginsModifier
 */
/**
 * Smarty number_format modifier plugin
 * Type:     modifier
 * Name:     number-format
 * Purpose:  php number_format wrapper to fix PHP 8.1 "Passing null to parameter #1 ($num) of type float is deprecated"
 *
 * @author Christian Meius <https://github.com/cmelius>
 *
 * @param string $num number to format
 * @param string $decimals  number of decimals
 * @param string $decimal_separator
 * @param string $thousands_separator
 *
 * @return string
 */
function smarty_modifier_number_format($num, $decimals = 0, $decimal_separator =".", $thousands_separator = ",")
{
    $num = null !== $num ? $num : 0;

    return number_format($num, $decimals, $decimal_separator, $thousands_separator);
}

<?php
/**
 * This file is part of the Smarty package.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Registers some helper/polyfill functions.
 */

const SMARTY_HELPER_FUNCTIONS_LOADED = true;

/**
 * Converts the first characters in $string to uppercase (A-Z) if it is an ASCII lowercase character (a-z).
 *
 * May not be required when running PHP8.2+: https://wiki.php.net/rfc/strtolower-ascii
 *
 * @param $string
 *
 * @return string
 */
function smarty_ucfirst_ascii($string): string {
    return smarty_strtoupper_ascii(substr($string, 0, 1)) . substr($string, 1);
}

/**
 * Converts all uppercase ASCII characters (A-Z) in $string to lowercase (a-z).
 *
 * May not be required when running PHP8.2+: https://wiki.php.net/rfc/strtolower-ascii
 *
 * @param $string
 *
 * @return string
 */
function smarty_strtolower_ascii($string): string {
    return strtr($string, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz');
}

/**
 * Converts all lowercase ASCII characters (a-z) in $string to uppercase (A-Z).
 *
 * May not be required when running PHP8.2+: https://wiki.php.net/rfc/strtolower-ascii
 *
 * @param $string
 *
 * @return string
 */
function smarty_strtoupper_ascii($string): string {
    return strtr($string, 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ');
}

/**
 * Polyfill for deprecated strftime() function (removed in PHP 8.1+).
 * 
 * PHP 8.1+: strftime() was deprecated and removed. This function provides
 * a compatibility layer by converting strftime format codes to date() format.
 * For full locale support, use IntlDateFormatter directly in your application.
 * 
 * @param string $format strftime format string
 * @param int|null $timestamp Unix timestamp (defaults to current time)
 * 
 * @return string|false Formatted date string or false on failure
 */
function smarty_strftime($format, $timestamp = null) {
    // Use current time if no timestamp provided
    if ($timestamp === null) {
        $timestamp = time();
    }
    
    // If the native strftime function still exists (PHP < 8.1), use it
    if (function_exists('strftime')) {
        return @strftime($format, $timestamp);
    }
    
    // PHP 8.1+: Convert strftime format to date() format
    // This is a basic conversion that handles most common format codes
    $strftimeToDate = [
        // Day
        '%d' => 'd',  // Day of month, 2 digits with leading zeros
        '%e' => 'j',  // Day of month, with leading space if single digit
        '%j' => 'z',  // Day of year, 3 digits with leading zeros
        '%u' => 'N',  // ISO-8601 numeric representation of day of week
        '%w' => 'w',  // Numeric representation of day of week
        
        // Week
        '%U' => '',   // Week number (Sunday as first day) - no direct equivalent
        '%V' => 'W',  // ISO-8601 week number
        '%W' => '',   // Week number (Monday as first day) - no direct equivalent
        
        // Month
        '%b' => 'M',  // Abbreviated month name
        '%B' => 'F',  // Full month name
        '%h' => 'M',  // Abbreviated month name (same as %b)
        '%m' => 'm',  // Month, 2 digits with leading zeros
        
        // Year
        '%C' => '',   // Century - no direct equivalent
        '%g' => 'o',  // ISO-8601 year (2 digits)
        '%G' => 'o',  // ISO-8601 year (4 digits)
        '%y' => 'y',  // Year, 2 digits
        '%Y' => 'Y',  // Year, 4 digits
        
        // Time
        '%H' => 'H',  // Hour, 24-hour format, 2 digits
        '%I' => 'h',  // Hour, 12-hour format, 2 digits
        '%l' => 'g',  // Hour, 12-hour format, no leading zero
        '%M' => 'i',  // Minutes, 2 digits
        '%p' => 'A',  // AM or PM
        '%P' => 'a',  // am or pm
        '%r' => 'h:i:s A',  // Time in 12-hour format with AM/PM
        '%R' => 'H:i',      // Time in 24-hour format HH:MM
        '%S' => 's',  // Seconds, 2 digits
        '%T' => 'H:i:s',    // Time in 24-hour format HH:MM:SS
        '%X' => 'H:i:s',    // Preferred time representation (no locale support)
        '%z' => 'O',  // Timezone offset
        '%Z' => 'T',  // Timezone abbreviation
        
        // Time and Date
        '%c' => 'D M d H:i:s Y',  // Preferred date and time (no locale support)
        '%D' => 'm/d/y',          // Date in US format (same as %m/%d/%y)
        '%F' => 'Y-m-d',          // Date in ISO 8601 format
        '%s' => 'U',              // Unix timestamp
        '%x' => 'm/d/y',          // Preferred date representation (no locale support)
        
        // Day names
        '%a' => 'D',  // Abbreviated weekday name
        '%A' => 'l',  // Full weekday name
        
        // Misc
        '%n' => "\n", // Newline
        '%t' => "\t", // Tab
        '%%' => '%',  // Literal %
    ];
    
    // Replace strftime format codes with date() format codes
    $dateFormat = str_replace(array_keys($strftimeToDate), array_values($strftimeToDate), $format);
    
    // Handle %e (day with leading space) specially since date() doesn't have exact equivalent
    if (strpos($format, '%e') !== false) {
        $day = date('j', $timestamp);
        $dateFormat = str_replace('%e', sprintf('%2d', $day), $format);
        $dateFormat = str_replace(array_keys($strftimeToDate), array_values($strftimeToDate), $dateFormat);
    }
    
    return date($dateFormat, $timestamp);
}
<?php
/**
 * Smarty plugin
 *
 * @package    Smarty
 * @subpackage PluginsModifier
 */
/**
 * Smarty date_format_intl modifier plugin
 * Type:     modifier
 * Name:     date_format_intl
 * Purpose:  format datestamps via IntlDateFormatter
 * Input:
 *          - string: input date string
 *          - format: ICU format for output
 *          - default_date: default date if $string is empty
 *
 * @link   https://www.smarty.net/manual/en/language.modifier.date.format.php date_format (Smarty online manual)
 * @author Monte Ohrt <monte at ohrt dot com>
 *
 * @param string $string       input date string
 * @param string $format       ICU format for output
 * @param string $default_date default date if $string is empty
 *
 * @return string |void
 * @uses   smarty_make_timestamp()
 */
function smarty_modifier_date_format_intl($string, $format = null, $default_date = '')
{
    if ($format === null) {
        $format = Smarty::$_ICU_DATE_FORMAT;
    }
    /**
     * require_once the {@link shared.make_timestamp.php} plugin
     */
    static $is_loaded = false;
    if (!$is_loaded) {
        if (!is_callable('smarty_make_timestamp')) {
            include_once SMARTY_PLUGINS_DIR . 'shared.make_timestamp.php';
        }
        $is_loaded = true;
    }
    if (!empty($string) && $string !== '0000-00-00' && $string !== '0000-00-00 00:00:00') {
        $timestamp = smarty_make_timestamp($string);
    } elseif (!empty($default_date)) {
        $timestamp = smarty_make_timestamp($default_date);
    } else {
        return;
    }
    if (!class_exists('IntlDateFormatter')) {
        return;
    }

    $formatter = new IntlDateFormatter(
        setlocale(LC_TIME, '0'),
        IntlDateFormatter::NONE,
        IntlDateFormatter::NONE
    );
    $formatter->setPattern($format);

    return $formatter->format($timestamp);
}

<?php
/**
 * Smarty plugin
 *
 * @package    Smarty
 * @subpackage PluginsModifier
 */
/**
 * Smarty date_format modifier plugin
 * Type:     modifier
 * Name:     date_format
 * Purpose:  format datestamps via strftime
 * Input:
 *          - string: input date string
 *          - format: strftime format for output
 *          - default_date: default date if $string is empty
 *
 * @link   http://www.smarty.net/manual/en/language.modifier.date.format.php date_format (Smarty online manual)
 * @author Monte Ohrt <monte at ohrt dot com>
 *
 * @param string $string       input date string
 * @param string $format       strftime format for output
 * @param string $default_date default date if $string is empty
 * @param string $formatter    either 'strftime' or 'auto'
 *
 * @return string |void
 * @uses   smarty_make_timestamp()
 */
function smarty_modifier_date_format($string, $format = null, $default_date = '', $formatter = 'auto')
{
    /**
     * Anonymous function, only used if version_compare >= PHP8.1
     *
     * Locale-formatted strftime using IntlDateFormatter (PHP 8.1 compatible)
     * This provides a cross-platform alternative to strftime() for when it will be removed from PHP.
     * Note that output can be slightly different between libc sprintf and this function as it is using ICU.
     * Usage:
     * use func \PHP81_BC\strftime;
     * echo strftime('%A %e %B %Y %X', new \DateTime('2021-09-28 00:00:00'), 'fr_FR');
     * Original use:
     * \setlocale('fr_FR.UTF-8', LC_TIME);
     * echo \strftime('%A %e %B %Y %X', strtotime('2021-09-28 00:00:00'));
     *
     * @param string                  $format    Date format
     * @param integer|string|DateTime $timestamp Timestamp
     *
     * @return string
     * @author BohwaZ <https://bohwaz.net/>
     * @quthor modified by https://github.com/cmelius
     */
    $php81strftime = function (string $format, $timestamp = null): string
    {

        if (null === $timestamp) {
            $timestamp = new \DateTime;
        } elseif (is_numeric($timestamp)) {
            $timestamp = date_create('@' . $timestamp);
        } elseif (is_string($timestamp)) {
            $timestamp = date_create('!' . $timestamp);
        }

        if (!($timestamp instanceof \DateTimeInterface)) {
            throw new \InvalidArgumentException('$timestamp argument is neither a valid UNIX timestamp, a valid date-time string or a DateTime object.');
        }

        $intl_formats = [
            '%a' => 'EEE',
            // An abbreviated textual representation of the day	Sun through Sat
            '%A' => 'EEEE',
            // A full textual representation of the day	Sunday through Saturday
            '%b' => 'MMM',
            // Abbreviated month name, based on the locale	Jan through Dec
            '%B' => 'MMMM',
            // Full month name, based on the locale	January through December
            '%h' => 'MMM',
            // Abbreviated month name, based on the locale (an alias of %b)	Jan through Dec
            '%p' => 'aa',
            // UPPER-CASE 'AM' or 'PM' based on the given time	Example: AM for 00:31, PM for 22:23
            '%P' => 'aa',
            // lower-case 'am' or 'pm' based on the given time	Example: am for 00:31, pm for 22:23
        ];

        $intl_formatter = function (\DateTimeInterface $timestamp, string $format) use ($intl_formats) {

            $tz = $timestamp->getTimezone();
            $date_type = IntlDateFormatter::FULL;
            $time_type = IntlDateFormatter::FULL;
            $pattern = '';

            // %c = Preferred date and time stamp based on locale
            // Example: Tue Feb 5 00:45:10 2009 for February 5, 2009 at 12:45:10 AM
            if ($format == '%c') {
                $date_type = IntlDateFormatter::LONG;
                $time_type = IntlDateFormatter::SHORT;
            }
            // %x = Preferred date representation based on locale, without the time
            // Example: 02/05/09 for February 5, 2009
            elseif ($format == '%x') {
                $date_type = IntlDateFormatter::SHORT;
                $time_type = IntlDateFormatter::NONE;
            } // Localized time format
            elseif ($format == '%X') {
                $date_type = IntlDateFormatter::NONE;
                $time_type = IntlDateFormatter::MEDIUM;
            } else {
                $pattern = $intl_formats[ $format ];
            }

            return (new IntlDateFormatter(null, $date_type, $time_type, $tz, null, $pattern))->format($timestamp);
        };

        // Same order as https://www.php.net/manual/en/function.strftime.php
        $translation_table = [
            // Day
            '%a' => $intl_formatter,
            '%A' => $intl_formatter,
            '%d' => 'd',
            '%e' => 'j',
            '%j' => function ($timestamp) {

                // Day number in year, 001 to 366
                return sprintf('%03d', $timestamp->format('z') + 1);
            },
            '%u' => 'N',
            '%w' => 'w',

            // Week
            '%U' => function ($timestamp) {

                // Number of weeks between date and first Sunday of year
                $day = new \DateTime(sprintf('%d-01 Sunday', $timestamp->format('Y')));

                return intval(($timestamp->format('z') - $day->format('z')) / 7);
            },
            '%W' => function ($timestamp) {

                // Number of weeks between date and first Monday of year
                $day = new \DateTime(sprintf('%d-01 Monday', $timestamp->format('Y')));

                return intval(($timestamp->format('z') - $day->format('z')) / 7);
            },
            '%V' => 'W',

            // Month
            '%b' => $intl_formatter,
            '%B' => $intl_formatter,
            '%h' => $intl_formatter,
            '%m' => 'm',

            // Year
            '%C' => function ($timestamp) {

                // Century (-1): 19 for 20th century
                return (int)$timestamp->format('Y') / 100;
            },
            '%g' => function ($timestamp) {

                return substr($timestamp->format('o'), -2);
            },
            '%G' => 'o',
            '%y' => 'y',
            '%Y' => 'Y',

            // Time
            '%H' => 'H',
            '%k' => 'G',
            '%I' => 'h',
            '%l' => 'g',
            '%M' => 'i',
            '%p' => $intl_formatter,
            // AM PM (this is reversed on purpose!)
            '%P' => $intl_formatter,
            // am pm
            '%r' => 'G:i:s A',
            // %I:%M:%S %p
            '%R' => 'H:i',
            // %H:%M
            '%S' => 's',
            '%X' => $intl_formatter,
            // Preferred time representation based on locale, without the date

            // Timezone
            '%z' => 'O',
            '%Z' => 'T',

            // Time and Date Stamps
            '%c' => $intl_formatter,
            '%D' => 'm/d/Y',
            '%F' => 'Y-m-d',
            '%s' => 'U',
            '%x' => $intl_formatter,
        ];

        $out = preg_replace_callback('/(?<!%)(%[a-zA-Z])/', function ($match) use ($translation_table, $timestamp) {

            if ($match[1] == '%n') {
                return "\n";
            } elseif ($match[1] == '%t') {
                return "\t";
            }

            if (!isset($translation_table[ $match[1] ])) {
                throw new \InvalidArgumentException(sprintf('Format "%s" is unknown in time format', $match[1]));
            }

            $replace = $translation_table[ $match[1] ];

            if (is_string($replace)) {
                return $timestamp->format($replace);
            } else {
                return $replace($timestamp, $match[1]);
            }
        }, $format);

        $out = str_replace('%%', '%', $out);

        return $out;
    };

    if ($format === null) {
        $format = Smarty::$_DATE_FORMAT;
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
    if ($formatter === 'strftime' || ($formatter === 'auto' && strpos($format, '%') !== false)) {
        if (Smarty::$_IS_WINDOWS) {
            $_win_from = array(
                '%D',
                '%h',
                '%n',
                '%r',
                '%R',
                '%t',
                '%T'
            );
            $_win_to = array(
                '%m/%d/%y',
                '%b',
                "\n",
                '%I:%M:%S %p',
                '%H:%M',
                "\t",
                '%H:%M:%S'
            );
            if (strpos($format, '%e') !== false) {
                $_win_from[] = '%e';
                $_win_to[] = sprintf('%\' 2d', date('j', $timestamp));
            }
            if (strpos($format, '%l') !== false) {
                $_win_from[] = '%l';
                $_win_to[] = sprintf('%\' 2d', date('h', $timestamp));
            }
            $format = str_replace($_win_from, $_win_to, $format);
        }

        if (version_compare(PHP_VERSION, '8.1', '>=')) {
            return $php81strftime($format, $timestamp);
        }
        return strftime($format, $timestamp);
    } else {
        return date($format, $timestamp);
    }
}

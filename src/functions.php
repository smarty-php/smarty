<?php
/**
 * This file is part of the Smarty package.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

use Smarty\Exception;

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
 * Function: smarty_make_timestamp
 * Purpose:  used by other smarty functions to make a timestamp from a string.
 *
 * @author Monte Ohrt <monte at ohrt dot com>
 *
 * @param DateTime|int|string $string date object, timestamp or string that can be converted using strtotime()
 *
 * @return int
 */
function smarty_make_timestamp($string)
{
	if (empty($string)) {
		// use "now":
		return time();
	} elseif ($string instanceof DateTime
		|| (interface_exists('DateTimeInterface', false) && $string instanceof DateTimeInterface)
	) {
		return (int)$string->format('U'); // PHP 5.2 BC
	} elseif (strlen($string) === 14 && ctype_digit($string)) {
		// it is mysql timestamp format of YYYYMMDDHHMMSS?
		return mktime(
			substr($string, 8, 2),
			substr($string, 10, 2),
			substr($string, 12, 2),
			substr($string, 4, 2),
			substr($string, 6, 2),
			substr($string, 0, 4)
		);
	} elseif (is_numeric($string)) {
		// it is a numeric string, we handle it as timestamp
		return (int)$string;
	} else {
		// strtotime should handle it
		$time = strtotime($string);
		if ($time === -1 || $time === false) {
			// strtotime() was not able to parse $string, use "now":
			return time();
		}
		return $time;
	}
}

/**
 * Multibyte string replace
 *
 * @param string|string[] $search  the string to be searched
 * @param string|string[] $replace the replacement string
 * @param string          $subject the source string
 * @param int             &$count  number of matches found
 *
 * @return string replaced string
 * @author Rodney Rehm
 */
function smarty_mb_str_replace($search, $replace, $subject, &$count = 0)
{
	if (!is_array($search) && is_array($replace)) {
		return false;
	}
	if (is_array($subject)) {
		// call mb_replace for each single string in $subject
		foreach ($subject as &$string) {
			$string = smarty_mb_str_replace($search, $replace, $string, $c);
			$count += $c;
		}
	} elseif (is_array($search)) {
		if (!is_array($replace)) {
			foreach ($search as &$string) {
				$subject = smarty_mb_str_replace($string, $replace, $subject, $c);
				$count += $c;
			}
		} else {
			$n = max(count($search), count($replace));
			while ($n--) {
				$subject = smarty_mb_str_replace(current($search), current($replace), $subject, $c);
				$count += $c;
				next($search);
				next($replace);
			}
		}
	} else {
		$mb_reg_charset = mb_regex_encoding();
		// Check if mbstring regex is using UTF-8
		$reg_is_unicode = !strcasecmp($mb_reg_charset, "UTF-8");
		if(!$reg_is_unicode) {
			// ...and set to UTF-8 if not
			mb_regex_encoding("UTF-8");
		}

		// See if charset used by Smarty is matching one used by regex...
		$current_charset = mb_regex_encoding();
		$convert_result = (bool)strcasecmp(\Smarty\Smarty::$_CHARSET, $current_charset);
		if($convert_result) {
			// ...convert to it if not.
			$subject = mb_convert_encoding($subject, $current_charset, \Smarty\Smarty::$_CHARSET);
			$search = mb_convert_encoding($search, $current_charset, \Smarty\Smarty::$_CHARSET);
			$replace = mb_convert_encoding($replace, $current_charset, \Smarty\Smarty::$_CHARSET);
		}

		$parts = mb_split(preg_quote($search), $subject ?? "") ?: array();
		// If original regex encoding was not unicode...
		if(!$reg_is_unicode) {
			// ...restore original regex encoding to avoid breaking the system.
			mb_regex_encoding($mb_reg_charset);
		}
		if($parts === false) {
			// This exception is thrown if call to mb_split failed.
			// Usually it happens, when $search or $replace are not valid for given mb_regex_encoding().
			// There may be other cases for it to fail, please file an issue if you find a reproducible one.
			throw new Exception("Source string is not a valid $current_charset sequence (probably)");
		}

		$count = count($parts) - 1;
		$subject = implode($replace, $parts);
		// Convert results back to charset used by Smarty, if needed.
		if($convert_result) {
			$subject = mb_convert_encoding($subject, \Smarty\Smarty::$_CHARSET, $current_charset);
		}
	}
	return $subject;
}
/**
 * escape_special_chars common function
 * Function: smarty_function_escape_special_chars
 * Purpose:  used by other smarty functions to escape
 *           special chars except for already escaped ones
 *
 * @author Monte Ohrt <monte at ohrt dot com>
 *
 * @param string $string text that should by escaped
 *
 * @return string
 */
function smarty_function_escape_special_chars($string)
{
	if (!is_array($string)) {
		$string = htmlspecialchars($string, ENT_COMPAT, \Smarty\Smarty::$_CHARSET, false);
	}
	return $string;
}

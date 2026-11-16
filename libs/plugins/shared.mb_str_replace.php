<?php
/**
 * Smarty shared plugin
 *
 * @package    Smarty
 * @subpackage PluginsShared
 */
if (!function_exists('smarty_mb_str_replace')) {
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
            $subject = $subject ?? '';
            $search = $search ?? '';
            $replace = $replace ?? '';
            // Byte-wise str_replace() is only safe on UTF-8, where a match cannot start in the middle of
            // a character. Other multibyte charsets (e.g. Shift_JIS) are converted to UTF-8 first.
            $convert_result = (bool)strcasecmp(Smarty::$_CHARSET, 'UTF-8');
            if($convert_result) {
                $subject = mb_convert_encoding($subject, 'UTF-8', Smarty::$_CHARSET);
                $search = mb_convert_encoding($search, 'UTF-8', Smarty::$_CHARSET);
                $replace = mb_convert_encoding($replace, 'UTF-8', Smarty::$_CHARSET);
            }

            if (!mb_check_encoding((string)$subject, 'UTF-8') || !mb_check_encoding((string)$search, 'UTF-8')) {
                // Keep the result of the former mb_split() based implementation for invalid input.
                $count = -1;
                return '';
            }

            $subject = str_replace($search, $replace, $subject, $count);
            // Convert results back to charset used by Smarty, if needed.
            if($convert_result) {
                $subject = mb_convert_encoding($subject, Smarty::$_CHARSET, 'UTF-8');
            }
        }
        return $subject;
    }
}

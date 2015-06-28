<?php

/**
 * Smarty Extension GetStreamVar
 *
 * getStreamVariable() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_GetStreamVar
{
    /**
     * gets  a stream variable
     *
     * @param         $obj
     * @param  string $variable the stream of the variable
     *
     * @return mixed
     * @throws \SmartyException
     */
    public static function getStreamVariable($obj, $variable)
    {
        $_result = '';
        $fp = fopen($variable, 'r+');
        if ($fp) {
            while (!feof($fp) && ($current_line = fgets($fp)) !== false) {
                $_result .= $current_line;
            }
            fclose($fp);

            return $_result;
        }
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        if ($smarty->error_unassigned) {
            throw new SmartyException('Undefined stream variable "' . $variable . '"');
        } else {
            return null;
        }
    }
}
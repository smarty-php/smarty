<?php
/**
 * Smarty Internal Plugin Function Call Handler
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */

/**
 * This class does call function defined with the {function} tag
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 */
class Smarty_Internal_Function_Call_Handler
{
    /**
     * This function handles calls to template functions defined by {function}
     * It does create a PHP function at the first call
     *
     * @param string                   $_name    template function name
     * @param Smarty_Internal_Template $_smarty_tpl
     * @param                          $_function
     * @param array                    $_params  Smarty variables passed as call parameter
     * @param bool                     $_nocache nocache flag
     *
     * @return bool
     */
    public static function call($_name, Smarty_Internal_Template $_smarty_tpl, $_function, $_params, $_nocache)
    {
        $funcParam = $_smarty_tpl->properties['tpl_function']['param'][$_name];
        $code = file_get_contents($funcParam['compiled_filepath']);
        if (preg_match("/\/\* {$_function} \*\/([\S\s]*?)\/\*\/ {$_function} \*\//", $code, $match)) {
            $output = "\n";
            $output .= $match[0];
            $output .= "?>\n";
        }
        unset($code, $match);
        eval($output);
        if (function_exists($_function)) {
            $_function ($_smarty_tpl, $_params);
            $tplPtr = $_smarty_tpl;
            while (isset($tplPtr->parent) && !isset($tplPtr->parent->cached)) {
                $tplPtr = $tplPtr->parent;
            }
            if (isset($tplPtr->parent->cached)) {
                $cached = $tplPtr->parent->cached;
            }
            return true;
        }
        return false;
    }
}

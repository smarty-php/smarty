<?php

/**
 * Smarty Extension GetVars
 *
 * getTemplateVars() and getVariable() methods
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_GetVars
{
    /**
     * Returns a single or all template variables
     *
     * @param          $obj
     * @param  string  $varname        variable name or null
     * @param  object  $_ptr           optional pointer to data object
     * @param  boolean $search_parents include parent templates?
     *
     * @return string variable value or or array of variables
     */
    public static function getTemplateVars($obj, $varname, $_ptr, $search_parents)
    {
        if (isset($varname)) {
            $_var = self::getVariable($obj, $varname, $_ptr, $search_parents, false);
            if (is_object($_var)) {
                return $_var->value;
            } else {
                return null;
            }
        } else {
            $_result = array();
            if ($_ptr === null) {
                $_ptr = $obj;
            }
            while ($_ptr !== null) {
                foreach ($_ptr->tpl_vars AS $key => $var) {
                    if (!array_key_exists($key, $_result)) {
                        $_result[$key] = $var->value;
                    }
                }
                // not found, try at parent
                if ($search_parents) {
                    $_ptr = $_ptr->parent;
                } else {
                    $_ptr = null;
                }
            }
            if ($search_parents && isset(Smarty::$global_tpl_vars)) {
                foreach (Smarty::$global_tpl_vars AS $key => $var) {
                    if (!array_key_exists($key, $_result)) {
                        $_result[$key] = $var->value;
                    }
                }
            }

            return $_result;
        }
    }

    /**
     * gets the object of a Smarty variable
     *
     * @param          $obj
     * @param  string  $variable       the name of the Smarty variable
     * @param  object  $_ptr           optional pointer to data object
     * @param  boolean $search_parents search also in parent data
     * @param bool     $error_enable
     *
     * @return object the object of the variable
     */
    public static function getVariable($obj, $variable, $_ptr, $search_parents, $error_enable)
    {
        if ($_ptr === null) {
            $_ptr = $obj;
        }
        while ($_ptr !== null) {
            if (isset($_ptr->tpl_vars[$variable])) {
                // found it, return it
                return $_ptr->tpl_vars[$variable];
            }
            // not found, try at parent
            if ($search_parents) {
                $_ptr = $_ptr->parent;
            } else {
                $_ptr = null;
            }
        }
        if (isset(Smarty::$global_tpl_vars[$variable])) {
            // found it, return it
            return Smarty::$global_tpl_vars[$variable];
        }
        $smarty = isset($obj->smarty) ? $obj->smarty : $obj;
        if ($smarty->error_unassigned && $error_enable) {
            // force a notice
            $x = $$variable;
        }

        return new Smarty_Undefined_Variable;
    }
}
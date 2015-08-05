<?php

/**
 * Smarty Extension Append
 *
 * getStreamVariable() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Extension_Append
{
    /**
     * appends values to template variables
     *
     * @param  array|string $tpl_var the template variable name(s)
     * @param  mixed        $value   the value to append
     * @param  boolean      $merge   flag if array elements shall be merged
     * @param  boolean      $nocache if true any output of this variable will be not cached
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public static function append($obj, $tpl_var, $value, $merge, $nocache)
    {
        if (is_array($tpl_var)) {
            // $tpl_var is an array, ignore $value
            foreach ($tpl_var as $_key => $_val) {
                if ($_key != '') {
                    self::append($obj, $_key, $_val, $merge, $nocache);
                }
            }
        } else {
            if ($tpl_var != '' && isset($value)) {
                if (!isset($obj->tpl_vars[$tpl_var])) {
                    $tpl_var_inst = Smarty_Internal_Extension_GetVars::getVariable($obj, $tpl_var, null, true, false);
                    if ($tpl_var_inst instanceof Smarty_Undefined_Variable) {
                        $obj->tpl_vars[$tpl_var] = new Smarty_Variable(null, $nocache);
                    } else {
                        $obj->tpl_vars[$tpl_var] = clone $tpl_var_inst;
                    }
                }
                if (!(is_array($obj->tpl_vars[$tpl_var]->value) || $obj->tpl_vars[$tpl_var]->value instanceof ArrayAccess)) {
                    settype($obj->tpl_vars[$tpl_var]->value, 'array');
                }
                if ($merge && is_array($value)) {
                    foreach ($value as $_mkey => $_mval) {
                        $obj->tpl_vars[$tpl_var]->value[$_mkey] = $_mval;
                    }
                } else {
                    $obj->tpl_vars[$tpl_var]->value[] = $value;
                }
            }
        }

        return $obj;
    }

    /**
     * appends values to template variables by reference
     *
     * @param  string  $tpl_var the template variable name
     * @param  mixed   &$value  the referenced value to append
     * @param  boolean $merge   flag if array elements shall be merged
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public static function appendByRef($obj, $tpl_var, &$value, $merge)
    {
        if ($tpl_var != '' && isset($value)) {
            if (!isset($obj->tpl_vars[$tpl_var])) {
                $obj->tpl_vars[$tpl_var] = new Smarty_Variable();
            }
            if (!is_array($obj->tpl_vars[$tpl_var]->value)) {
                settype($obj->tpl_vars[$tpl_var]->value, 'array');
            }
            if ($merge && is_array($value)) {
                foreach ($value as $_key => $_val) {
                    $obj->tpl_vars[$tpl_var]->value[$_key] = &$value[$_key];
                }
            } else {
                $obj->tpl_vars[$tpl_var]->value[] = &$value;
            }
        }

        return $obj;
    }
}
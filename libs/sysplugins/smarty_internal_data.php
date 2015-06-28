<?php
/**
 * Smarty Internal Plugin Data
 * This file contains the basic classes and methods for template and variable creation
 *
 * @package    Smarty
 * @subpackage Template
 * @author     Uwe Tews
 */

/**
 * Base class with template and variable methods
 *
 * @package    Smarty
 * @subpackage Template
 */
class Smarty_Internal_Data
{
    /**
     * name of class used for templates
     *
     * @var string
     */
    public $template_class = 'Smarty_Internal_Template';
    /**
     * template variables
     *
     * @var array
     */
    public $tpl_vars = array();
    /**
     * parent template (if any)
     *
     * @var Smarty_Internal_Template
     */
    public $parent = null;
    /**
     * configuration settings
     *
     * @var array
     */
    public $config_vars = array();

    /**
     * assigns a Smarty variable
     *
     * @param  array|string $tpl_var the template variable name(s)
     * @param  mixed        $value   the value to assign
     * @param  boolean      $nocache if true any output of this variable will be not cached
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function assign($tpl_var, $value = null, $nocache = false)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $_key => $_val) {
                if ($_key != '') {
                    $this->tpl_vars[$_key] = new Smarty_Variable($_val, $nocache);
                }
            }
        } else {
            if ($tpl_var != '') {
                $this->tpl_vars[$tpl_var] = new Smarty_Variable($value, $nocache);
            }
        }

        return $this;
    }

    /**
     * assigns a global Smarty variable
     *
     * @param  string  $varname the global variable name
     * @param  mixed   $value   the value to assign
     * @param  boolean $nocache if true any output of this variable will be not cached
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function assignGlobal($varname, $value = null, $nocache = false)
    {
        if ($varname != '') {
            Smarty::$global_tpl_vars[$varname] = new Smarty_Variable($value, $nocache);
            $ptr = $this;
            while ($ptr instanceof Smarty_Internal_Template) {
                $ptr->tpl_vars[$varname] = clone Smarty::$global_tpl_vars[$varname];
                $ptr = $ptr->parent;
            }
        }

        return $this;
    }

    /**
     * assigns values to template variables by reference
     *
     * @param string   $tpl_var the template variable name
     * @param          $value
     * @param  boolean $nocache if true any output of this variable will be not cached
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function assignByRef($tpl_var, &$value, $nocache = false)
    {
        if ($tpl_var != '') {
            $this->tpl_vars[$tpl_var] = new Smarty_Variable(null, $nocache);
            $this->tpl_vars[$tpl_var]->value = &$value;
        }

        return $this;
    }

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
    public function append($tpl_var, $value = null, $merge = false, $nocache = false)
    {
        Smarty_Internal_Extension_Append::append($this, $tpl_var, $value, $merge, $nocache);
        return $this;
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
    public function appendByRef($tpl_var, &$value, $merge = false)
    {
        Smarty_Internal_Extension_Append::appendByRef($this, $tpl_var, $value, $merge);
        return $this;
    }

    /**
     * Returns a single or all template variables
     *
     * @param  string  $varname        variable name or null
     * @param  object  $_ptr           optional pointer to data object
     * @param  boolean $search_parents include parent templates?
     *
     * @return string  variable value or or array of variables
     */
    public function getTemplateVars($varname = null, $_ptr = null, $search_parents = true)
    {
        return Smarty_Internal_Extension_GetVars::getTemplateVars($this, $varname, $_ptr, $search_parents);
    }

    /**
     * clear the given assigned template variable.
     *
     * @param  string|array $tpl_var the template variable(s) to clear
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function clearAssign($tpl_var)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $curr_var) {
                unset($this->tpl_vars[$curr_var]);
            }
        } else {
            unset($this->tpl_vars[$tpl_var]);
        }

        return $this;
    }

    /**
     * clear all the assigned template variables.
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function clearAllAssign()
    {
        $this->tpl_vars = array();

        return $this;
    }

    /**
     * load a config file, optionally load just selected sections
     *
     * @param  string $config_file filename
     * @param  mixed  $sections    array of section names, single section or null
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function configLoad($config_file, $sections = null)
    {
        // load Config class
        Smarty_Internal_Extension_Config::configLoad($this, $config_file, $sections);
        return $this;
    }

    /**
     * gets the object of a Smarty variable
     *
     * @param  string  $variable       the name of the Smarty variable
     * @param  object  $_ptr           optional pointer to data object
     * @param  boolean $search_parents search also in parent data
     * @param bool     $error_enable
     *
     * @return object  the object of the variable
     */
    public function getVariable($variable, $_ptr = null, $search_parents = true, $error_enable = true)
    {
        return Smarty_Internal_Extension_GetVars::getVariable($this, $variable, $_ptr, $search_parents, $error_enable);
    }

    /**
     * gets  a config variable
     *
     * @param  string $variable the name of the config variable
     * @param bool    $error_enable
     *
     * @return mixed  the value of the config variable
     */
    public function getConfigVariable($variable, $error_enable = true)
    {
        return Smarty_Internal_Extension_Config::getConfigVariable($this, $variable, $error_enable = true);
    }

    /**
     * Returns a single or all config variables
     *
     * @param  string $varname variable name or null
     * @param bool    $search_parents
     *
     * @return string variable value or or array of variables
     */
    public function getConfigVars($varname = null, $search_parents = true)
    {
        return Smarty_Internal_Extension_Config::getConfigVars($this, $varname, $search_parents);
    }

    /**
     * Deassigns a single or all config variables
     *
     * @param  string $varname variable name or null
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for chaining
     */
    public function clearConfig($varname = null)
    {
        return Smarty_Internal_Extension_Config::clearConfig($this, $varname);
    }

    /**
     * gets  a stream variable
     *
     * @param  string $variable the stream of the variable
     *
     * @throws SmartyException
     * @return mixed  the value of the stream variable
     */
    public function getStreamVariable($variable)
    {
        return Smarty_Internal_Extension_GetStreamVar::getStreamVariable($this, $variable);
    }
}

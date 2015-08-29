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
 *
 * @method mixed getConfigVars(string $varname = null, bool $search_parents = true)
 * @method mixed getStreamVariable(string $variable)
 * @local_method mixed getTemplateVars(string $varname = null, Smarty_Internal_Data $_ptr = null, bool $search_parents = true)
 * @method Smarty_Internal_Data clearAssign(mixed $tpl_var)
 * @method Smarty_Internal_Data clearAllAssign()
 * @method Smarty_Internal_Data clearConfig(string $varname = null)
 * @method Smarty_Internal_Data configLoad(string $config_file, mixed $sections = null, string $scope = 'local')
 * @property int $_objType
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
     * @var Smarty|Smarty_Internal_Template|Smarty_Internal_Data
     */
    public $parent = null;

    /**
     * configuration settings
     *
     * @var array
     */
    public $config_vars = array();

    /**
     * Cache for property information from generic getter/setter
     * Preloaded with names which should not use with generic getter/setter
     *
     * @var array
     */
    private $_property_info = array('AutoloadFilters' => 0, 'DefaultModifiers' => 0, 'ConfigVars' => 0,
                                    'DebugTemplate'   => 0, 'RegisteredObject' => 0, 'StreamVariable' => 0,
                                    'TemplateVars'    => 0,);

    /**
     * assigns a Smarty variable
     *
     * @param  array|string $tpl_var the template variable name(s)
     * @param  mixed        $value   the value to assign
     * @param  boolean      $nocache if true any output of this variable will be not cached
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for
     *                              chaining
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
     * appends values to template variables
     *
     * @api  Smarty::append()
     * @link http://www.smarty.net/docs/en/api.append.tpl
     *
     * @param  array|string $tpl_var                                           the template variable name(s)
     * @param  mixed        $value                                             the value to append
     * @param  bool         $merge                                             flag if array elements shall be merged
     * @param  bool         $nocache                                           if true any output of this variable will
     *                                                                         be not cached
     *
     * @return \Smarty_Internal_Data|\Smarty_Internal_Template|\Smarty
     */
    public function append($tpl_var, $value = null, $merge = false, $nocache = false)
    {
        if (is_array($tpl_var)) {
            // $tpl_var is an array, ignore $value
            foreach ($tpl_var as $_key => $_val) {
                if ($_key != '') {
                    $this->append($_key, $_val, $merge, $nocache);
                }
            }
        } else {
            if ($tpl_var != '' && isset($value)) {
                if (!isset($this->tpl_vars[$tpl_var])) {
                    $tpl_var_inst = $this->_getVariable($tpl_var, null, true, false);
                    if ($tpl_var_inst instanceof Smarty_Undefined_Variable) {
                        $this->tpl_vars[$tpl_var] = new Smarty_Variable(null, $nocache);
                    } else {
                        $this->tpl_vars[$tpl_var] = clone $tpl_var_inst;
                    }
                }
                if (!(is_array($this->tpl_vars[$tpl_var]->value) ||
                    $this->tpl_vars[$tpl_var]->value instanceof ArrayAccess)
                ) {
                    settype($this->tpl_vars[$tpl_var]->value, 'array');
                }
                if ($merge && is_array($value)) {
                    foreach ($value as $_mkey => $_mval) {
                        $this->tpl_vars[$tpl_var]->value[$_mkey] = $_mval;
                    }
                } else {
                    $this->tpl_vars[$tpl_var]->value[] = $value;
                }
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
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for
     *                              chaining
     */
    public function assignGlobal($varname, $value = null, $nocache = false)
    {
        if ($varname != '') {
            Smarty::$global_tpl_vars[$varname] = new Smarty_Variable($value, $nocache);
            $ptr = $this;
            while ($ptr->_objType == 2) {
                $ptr->tpl_vars[$varname] = clone Smarty::$global_tpl_vars[$varname];
                $ptr = $ptr->parent;
            }
        }
        return $this;
    }

    /**
     * appends values to template variables by reference
     *
     * @param  string  $tpl_var the template variable name
     * @param  mixed   &$value  the referenced value to append
     * @param  boolean $merge   flag if array elements shall be merged
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for
     *                              chaining
     */
    public function appendByRef($tpl_var, &$value, $merge = false)
    {
        Smarty_Internal_Method_AppendByRef::appendByRef($this, $tpl_var, $value, $merge);
        return $this;
    }

    /**
     * assigns values to template variables by reference
     *
     * @param string   $tpl_var the template variable name
     * @param          $value
     * @param  boolean $nocache if true any output of this variable will be not cached
     *
     * @return Smarty_Internal_Data current Smarty_Internal_Data (or Smarty or Smarty_Internal_Template) instance for
     *                              chaining
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
     * Returns a single or all template variables
     *
     * @api  Smarty::getTemplateVars()
     * @link http://www.smarty.net/docs/en/api.get.template.vars.tpl
     *
      * @param  string                                                 $varname        variable name or null
     * @param \Smarty_Internal_Data|\Smarty_Internal_Template|\Smarty $_ptr           optional pointer to data object
     * @param  bool                                                   $search_parents include parent templates?
     *
     * @return mixed variable value or or array of variables
     */
    public function getTemplateVars($varname = null, Smarty_Internal_Data $_ptr = null, $search_parents = true)
    {
        if (isset($varname)) {
            $_var = $this->_getVariable($varname, $_ptr, $search_parents, false);
            if (is_object($_var)) {
                return $_var->value;
            } else {
                return null;
            }
        } else {
            $_result = array();
            if ($_ptr === null) {
                $_ptr = $this;
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
     * @param  string                                                 $variable       the name of the Smarty variable
     * @param \Smarty_Internal_Data|\Smarty_Internal_Template|\Smarty $_ptr           optional pointer to data object
     * @param  bool                                                   $search_parents search also in parent data
     * @param bool                                                    $error_enable
     *
     * @return \Smarty_Variable
     */
    public function _getVariable($variable, Smarty_Internal_Data $_ptr = null, $search_parents = true, $error_enable = true)
    {
        if ($_ptr === null) {
            $_ptr = $this;
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
        /* @var \Smarty $smarty */
        $smarty = isset($this->smarty) ? $this->smarty : $this;
        if ($smarty->error_unassigned && $error_enable) {
            // force a notice
            $x = $$variable;
        }

        return new Smarty_Undefined_Variable;
    }

    /**
     * gets  a config variable value
     *
     * @param  string $variable the name of the config variable
     * @param bool    $error_enable
     *
     * @return mixed  the value of the config variable
     */
    public function _getConfigVariable($variable, $error_enable = true)
    {
        $_ptr = $this;
        while ($_ptr !== null) {
            if (isset($_ptr->config_vars[$variable])) {
                // found it, return it
                return $_ptr->config_vars[$variable];
            }
            // not found, try at parent
            $_ptr = $_ptr->parent;
        }
        /* @var \Smarty $smarty */
        $smarty = isset($this->smarty) ? $this->smarty : $this;
        if ($smarty->error_unassigned && $error_enable) {
            // force a notice
            $x = $$variable;
        }
        return null;
    }

    /**
     * Handle unknown class methods
     *
     * @param string $name unknown method-name
     * @param array  $args argument array
     *
     * @return mixed
     * @throws SmartyException
     */
    public function __call($name, $args)
    {
        $smarty = $this->_objType == 1 ? $this : $this->smarty;
        if (!isset($smarty->_cache['extObjCache'][$name])) {

            $class = 'Smarty_Internal_Method_' . ucfirst($name);
            if (preg_match('/^(set|get)([A-Z].*)$/', $name, $match)) {
                if (!isset($this->_property_info[$prop = $match[2]])) {
                    if (!isset($this->_property_info[$prop])) {
                        // convert camel case to underscored name
                        $smarty->_cache['resolvedProp'][$prop] = $pn = strtolower(join('_', preg_split('/([A-Z][^A-Z]*)/', $prop, - 1, PREG_SPLIT_NO_EMPTY |
                                                                                                                  PREG_SPLIT_DELIM_CAPTURE)));
                        $this->_property_info[$prop] = property_exists($this, $pn) ? 1 : ($this->_objType == 2 &&
                        property_exists($smarty, $pn) ? 2 : 0);
                    }
                }
                if ($this->_property_info[$prop]) {
                    $pn = $smarty->_cache['resolvedProp'][$prop];
                    if ($match[1] == 'get') {
                        return $this->_property_info[$prop] == 1 ? $this->$pn : $this->smarty->$pn;
                    } else {
                        return $this->_property_info[$prop] ==
                        1 ? $this->$pn = $args[0] : $this->smarty->$pn = $args[0];
                    }
                } elseif (!class_exists($class)) {
                    throw new SmartyException("property '$pn' does not exist.");
                }
            }
            if (class_exists($class)) {
                $callback = array($smarty->_cache['extObjCache'][$name] = new $class(), $name);
            }
        } else {
            $callback = array($smarty->_cache['extObjCache'][$name], $name);
        }
        if (isset($callback) && $callback[0]->objMap | $this->_objType) {
            array_unshift($args, $this);
            return call_user_func_array($callback, $args);
        }
        throw new SmartyException("method '$name' does not exist.");
    }
}

<?php

namespace Smarty;

/**
 * Smarty Internal Plugin Data
 * This file contains the basic classes and methods for template and variable creation
 *


 * @author     Uwe Tews
 */

/**
 * Base class with template and variable methods
 */
abstract class Data
{

	/**
	 * define variable scopes
	 */
	const SCOPE_LOCAL    = 1;
	const SCOPE_PARENT   = 2;
	const SCOPE_TPL_ROOT = 4;
	const SCOPE_ROOT     = 8;
	const SCOPE_SMARTY   = 16;
	const SCOPE_GLOBAL   = 32;

	/**
	 * Global smarty instance
	 *
	 * @var Smarty
	 */
	public $smarty = null;

    /**
     * template variables
     *
     * @var Variable[]
     */
    public $tpl_vars = array();

    /**
     * parent data container (if any)
     *
     * @var Data
     */
    public $parent = null;

    /**
     * configuration settings
     *
     * @var string[]
     */
    public $config_vars = array();

    /**
     * assigns a Smarty variable
     *
     * @param array|string $tpl_var the template variable name(s)
     * @param mixed        $value   the value to assign
     * @param boolean      $nocache if true any output of this variable will be not cached
     *
     * @return Data current Data (or Smarty or \Smarty\Template) instance for
     *                              chaining
     */
    public function assign($tpl_var, $value = null, $nocache = false)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $_key => $_val) {
                $this->assign($_key, $_val, $nocache);
            }
        } else {
	        $this->tpl_vars[ $tpl_var ] = new Variable($value, $nocache);
        }
        return $this;
    }

    /**
     * appends values to template variables
     *
     * @param array|string $tpl_var the template variable name(s)
     * @param mixed        $value   the value to append
     * @param bool         $merge   flag if array elements shall be merged
     * @param bool         $nocache if true any output of this variable will
     *                              be not cached
     *
     * @return Data
     * @link https://www.smarty.net/docs/en/api.append.tpl
     *
     * @api  Smarty::append()
     */
	public function append($tpl_var, $value = null, $merge = false, $nocache = false)
	{
		if (is_array($tpl_var)) {
			foreach ($tpl_var as $_key => $_val) {
				$this->append($_key, $_val, $merge, $nocache);
			}
		} else {

			$newValue = $this->getValue($tpl_var) ?? [];
			if (!is_array($newValue)) {
				$newValue = (array) $newValue;
			}

			if ($merge && is_array($value)) {
				foreach ($value as $_mkey => $_mval) {
					$newValue[$_mkey] = $_mval;
				}
			} else {
				$newValue[] = $value;
			}

			$this->assign($tpl_var, $newValue, $nocache);
		}
		return $this;
	}

    /**
     * assigns a global Smarty variable
     *
     * @param string  $varName the global variable name
     * @param mixed   $value   the value to assign
     * @param boolean $nocache if true any output of this variable will be not cached
     *
     * @return Data
     */
    public function assignGlobal($varName, $value = null, $nocache = false)
    {
	    return $this->_getSmartyObj()->assign($varName, $value, $nocache);
    }

    /**
     * Returns a single or all template variables
     *
     * @param string                                                  $varName       variable name or null
     * @param bool $searchParents include parent templates?
     *
     * @return mixed variable value or or array of variables
     * @api  Smarty::getTemplateVars()
     * @link https://www.smarty.net/docs/en/api.get.template.vars.tpl
     *
     */
    public function getTemplateVars($varName = null, $searchParents = true)
    {
	    if (isset($varName)) {
			return $this->getValue($varName, $searchParents);
		    $_var = $_ptr->getVariable($varName, $searchParents, false);
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
			    foreach ($_ptr->tpl_vars as $key => $var) {
				    if (!array_key_exists($key, $_result)) {
					    $_result[ $key ] = $var->value;
				    }
			    }
			    // not found, try at parent
			    if ($searchParents && isset($_ptr->parent)) {
				    $_ptr = $_ptr->parent;
			    } else {
				    $_ptr = null;
			    }
		    }
		    if ($searchParents) {
			    foreach ($this->_getSmartyObj()->getAllGlobalTemplateVars() as $key => $var) {
				    if (!array_key_exists($key, $_result)) {
					    $_result[ $key ] = $var->value;
				    }
			    }
		    }
		    return $_result;
	    }
    }

	/**
	 * Wrapper for ::getVariable()
	 *
	 * @deprecated since 5.0
	 *
	 * @param $varName
	 * @param $searchParents
	 * @param $errorEnable
	 *
	 * @return void
	 */
	public function _getVariable($varName, $searchParents = true, $errorEnable = true) {
		trigger_error('Using ::_getVariable() to is deprecated and will be ' .
			'removed in a future release. Use getVariable() instead.', E_USER_DEPRECATED);
		return $this->getVariable($varName, $searchParents, $errorEnable);
	}

	/**
	 * Gets the object of a Smarty variable
	 *
	 * @param string $varName the name of the Smarty variable
	 * @param bool $searchParents search also in parent data
	 * @param bool $errorEnable
	 *
	 * @return Variable
	 */
	public function getVariable($varName, $searchParents = true, $errorEnable = true) {
		if (isset($this->tpl_vars[$varName])) {
			return $this->tpl_vars[$varName];
		}

		if ($this->parent) {
			return $this->parent->getVariable($varName, $searchParents, $errorEnable);
		}

		if ($errorEnable && $this->_getSmartyObj()->error_unassigned) {
			// force a notice
			$x = $$varName;
		}
		return new UndefinedVariable();
	}

	/**
	 * Indicates if given variable has been set.
	 * @param $varName
	 *
	 * @return bool
	 */
	public function hasVariable($varName): bool {
		return !($this->getVariable($varName) instanceof UndefinedVariable);
	}

	/**
	 * Returns the value of the Smarty\Variable given by $varName, or null if the variable does not exist.
	 *
	 * @param $varName
	 * @param bool $searchParents
	 *
	 * @return mixed|null
	 */
	public function getValue($varName, $searchParents = true) {
		$variable = $this->getVariable($varName, $searchParents);
		return isset($variable) ? $variable->getValue() : null;
	}

    /**
     * Get Smarty object
     *
     * @return Smarty
     */
    public function _getSmartyObj()
    {
        return $this->smarty;
    }

	/**
	 * clear the given assigned template variable(s).
	 *
	 * @param string|array $tpl_var the template variable(s) to clear
	 *
	 * @return Data
	 * @link https://www.smarty.net/docs/en/api.clear.assign.tpl
	 *
	 * @api  Smarty::clearAssign()
	 */
	public function clearAssign($tpl_var)
	{
		if (is_array($tpl_var)) {
			foreach ($tpl_var as $curr_var) {
				unset($this->tpl_vars[ $curr_var ]);
			}
		} else {
			unset($this->tpl_vars[ $tpl_var ]);
		}
		return $this;
	}

	/**
	 * clear all the assigned template variables.
	 *
	 * @return Data
	 * @link https://www.smarty.net/docs/en/api.clear.all.assign.tpl
	 *
	 * @api  Smarty::clearAllAssign()
	 */
	public function clearAllAssign()
	{
		$this->tpl_vars = array();
		return $this;
	}

	/**
	 * clear a single or all config variables
	 *
	 * @param string|null $name variable name or null
	 *
	 * @return Data
	 * @link https://www.smarty.net/docs/en/api.clear.config.tpl
	 *
	 * @api  Smarty::clearConfig()
	 */
	public function clearConfig($name = null)
	{
		if (isset($name)) {
			unset($this->config_vars[ $name ]);
		} else {
			$this->config_vars = array();
		}
		return $this;
	}





	/**
	 * Gets a config variable value
	 *
	 * @param null $varName the name of the config variable
	 *
	 * @return mixed  the value of the config variable
	 * @throws Exception
	 */
	public function getConfigVariable($varName = null)
	{

		if (isset($this->config_vars[$varName])) {
			return $this->config_vars[$varName];
		}

		$returnValue = $this->parent ? $this->parent->getConfigVariable($varName) : null;

		if ($returnValue === null && $this->_getSmartyObj()->error_unassigned) {
			throw new Exception("Undefined variable $varName");
		}

		return $returnValue;
	}

	/**
	 * Returns a single or all config variables
	 *
	 * @api  Smarty::getConfigVars()
	 * @link https://www.smarty.net/docs/en/api.get.config.vars.tpl
	 *
	 * @param string                                                  $varname        variable name or null
	 *
	 * @return mixed variable value or or array of variables
	 */
	public function getConfigVars($varname = null)
	{
		if (isset($varname)) {
			return $this->getConfigVariable($varname);
		}

		return array_merge($this->parent ? $this->parent->getConfigVars() : [], $this->config_vars);
	}

	/**
	 * Returns a single or all global  variables
	 *
	 * @api Smarty::getGlobal()
	 *
	 * @param string                $varName variable name or null
	 *
	 * @return string|array variable value or or array of variables
	 */
	public function getGlobal($varName = null)
	{
		if (isset($varName)) {
			if ($this->_getSmartyObj()->getGlobalVariable($varName)) {
				return $this->_getSmartyObj()->getGlobalVariable($varName)->getValue();
			} else {
				return '';
			}
		} else {
			$_result = [];
			foreach ($this->_getSmartyObj()->getAllGlobalTemplateVars() as $key => $var) {
				$_result[ $key ] = $var->value;
			}
			return $_result;
		}
	}



}

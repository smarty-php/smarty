<?php

namespace Smarty;

/**
 * Smarty Internal Plugin Data
 * This file contains the basic properties and methods for holding config and template variables
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
	 * @param mixed $value the value to assign
	 * @param boolean $nocache if true any output of this variable will be not cached
	 * @param int $scope one of self::SCOPE_* constants
	 *
	 * @return Data current Data (or Smarty or \Smarty\Template) instance for
	 *                              chaining
	 */
    public function assign($tpl_var, $value = null, $nocache = false, $scope = 0)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $_key => $_val) {
                $this->assign($_key, $_val, $nocache, $scope);
            }
        }

		switch ($scope) {
			case self::SCOPE_GLOBAL:
			case self::SCOPE_SMARTY:
				$this->_getSmartyObj()->assign($tpl_var, $value);
				break;
			case self::SCOPE_TPL_ROOT:
				$ptr = $this;
				while (isset($ptr->parent) && ($ptr->parent instanceof Template)) {
					$ptr = $ptr->parent;
				}
				$ptr->assign($tpl_var, $value);
				break;
			case self::SCOPE_ROOT:
				$ptr = $this;
				while (isset($ptr->parent) && !($ptr->parent instanceof Smarty)) {
					$ptr = $ptr->parent;
				}
				$ptr->assign($tpl_var, $value);
				break;
			case self::SCOPE_PARENT:
				if ($this->parent) {
					$this->parent->assign($tpl_var, $value);
				} else {
					// assign local as fallback
					$this->assign($tpl_var, $value);
				}
				break;
			default:
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
     * @deprecated since 5.0
     */
    public function assignGlobal($varName, $value = null, $nocache = false)
    {
		trigger_error(__METHOD__ . " is deprecated. Use \\Smarty\\Smarty::assign() to assign a variable " .
		" at the Smarty level.", E_USER_DEPRECATED);
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
	    }

		return array_merge($this->parent && $searchParents ? $this->parent->getTemplateVars() : [], $this->tpl_vars);
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
	 * load config variables into template object
	 *
	 * @param array $new_config_vars
	 */
	public function assignConfigVars($new_config_vars) {

		// copy global config vars
		foreach ($new_config_vars['vars'] as $variable => $value) {
			if ($this->smarty->config_overwrite || !isset($this->config_vars[$variable])) {
				$this->config_vars[$variable] = $value;
			} else {
				$this->config_vars[$variable] = array_merge((array)$this->config_vars[$variable], (array)$value);
			}
		}
		// scan sections
		$sections = $this->source->config_sections;
		if (!empty($sections)) {
			foreach ((array)$sections as $tpl_section) {
				if (isset($new_config_vars['sections'][$tpl_section])) {
					foreach ($new_config_vars['sections'][$tpl_section]['vars'] as $variable => $value) {
						if ($this->smarty->config_overwrite || !isset($this->config_vars[$variable])) {
							$this->config_vars[$variable] = $value;
						} else {
							$this->config_vars[$variable] = array_merge((array)$this->config_vars[$variable], (array)$value);
						}
					}
				}
			}
		}
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
	 * @param string $varName the name of the config variable
	 *
	 * @return mixed  the value of the config variable
	 * @throws Exception
	 */
	public function getConfigVariable($varName)
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
	 * @param string $varname variable name or null
	 *
	 * @return mixed variable value or or array of variables
	 * @throws Exception
	 * @link https://www.smarty.net/docs/en/api.get.config.vars.tpl
	 *
	 * @api  Smarty::getConfigVars()
	 */
	public function getConfigVars($varname = null)
	{
		if (isset($varname)) {
			return $this->getConfigVariable($varname);
		}

		return array_merge($this->parent ? $this->parent->getConfigVars() : [], $this->config_vars);
	}

	/**
	 * load a config file, optionally load just selected sections
	 *
	 * @param string                                                  $config_file filename
	 * @param mixed                                                   $sections    array of section names, single
	 *                                                                             section or null
	 *
	 * @return $this
	 * @throws \Exception
	 *@api  Smarty::configLoad()
	 * @link https://www.smarty.net/docs/en/api.config.load.tpl
	 *
	 */
	public function configLoad($config_file, $sections = null)
	{
		$this->_loadConfigfile($config_file, $sections);
		return $this;
	}

	/**
	 * load a config file, optionally load just selected sections
	 *
	 * @param string $config_file filename
	 * @param mixed                                                   $sections    array of section names, single
	 *                                                                             section or null

	 * @returns Template
	 * @throws \Exception
	 * @link https://www.smarty.net/docs/en/api.config.load.tpl
	 *
	 * @api  Smarty::configLoad()
	 */
	protected function _loadConfigfile($config_file, $sections = null)
	{
		$smarty = $this->_getSmartyObj();

		$confObj = new Template($config_file, $smarty, $this, null, null, null, null, true);
		$confObj->caching = Smarty::CACHING_OFF;
		$confObj->source->config_sections = $sections;
		$confObj->compiled = \Smarty\Template\Compiled::load($confObj);
		$confObj->compiled->render($confObj);
		return $confObj;
	}


}

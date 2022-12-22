<?php

namespace Smarty;

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
 */
abstract class Data
{

	/**
	 * Global smarty instance
	 *
	 * @var Smarty
	 */
	public $smarty = null;

    /**
     * This object type (Smarty = 1, template = 2, data = 4)
     *
     * @var int
     */
    public $_objType = 4;

    /**
     * name of class used for templates
     *
     * @var string
     */
    public $template_class = 'Smarty\Template';

    /**
     * template variables
     *
     * @var Variable[]
     */
    public $tpl_vars = array();

    /**
     * parent template (if any)
     *
     * @var Smarty|Template|DataObject
     */
    public $parent = null;

    /**
     * configuration settings
     *
     * @var string[]
     */
    public $config_vars = array();

    /**
     * \Smarty\Data constructor.
     */
    public function __construct()
    {
    }

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
            if ($tpl_var !== '') {
                if ($this->_objType === 2) {
                    /**
                     *
                     *
                     * @var Template $this
                     */
                    $this->_assignInScope($tpl_var, $value, $nocache);
                } else {
                    $this->tpl_vars[ $tpl_var ] = new Variable($value, $nocache);
                }
            }
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
			// $tpl_var is an array, ignore $value
			foreach ($tpl_var as $_key => $_val) {
				if ($_key !== '') {
					$this->append($this, $_key, $_val, $merge, $nocache);
				}
			}
		} else {
			if ($tpl_var !== '' && isset($value)) {
				if (!isset($this->tpl_vars[ $tpl_var ])) {
					$tpl_var_inst = $this->_getVariable($tpl_var, null, true, false);
					if ($tpl_var_inst instanceof UndefinedVariable) {
						$this->tpl_vars[ $tpl_var ] = new \Smarty\Variable(null, $nocache);
					} else {
						$this->tpl_vars[ $tpl_var ] = clone $tpl_var_inst;
					}
				}
				if (!(is_array($this->tpl_vars[ $tpl_var ]->value)
					|| $this->tpl_vars[ $tpl_var ]->value instanceof ArrayAccess)
				) {
					settype($this->tpl_vars[ $tpl_var ]->value, 'array');
				}
				if ($merge && is_array($value)) {
					foreach ($value as $_mkey => $_mval) {
						$this->tpl_vars[ $tpl_var ]->value[ $_mkey ] = $_mval;
					}
				} else {
					$this->tpl_vars[ $tpl_var ]->value[] = $value;
				}
			}
			$this->_updateScope($tpl_var);
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
	    if ($varName !== '') {
		    Smarty::$global_tpl_vars[ $varName ] = new \Smarty\Variable($value, $nocache);
		    $ptr = $this;
		    while ($ptr->_isTplObj()) {
			    $ptr->tpl_vars[ $varName ] = clone Smarty::$global_tpl_vars[ $varName ];
			    $ptr = $ptr->parent;
		    }
	    }
	    return $this;
    }

    /**
     * appends values to template variables by reference
     *
     * @param string  $tpl_var the template variable name
     * @param mixed   &$value  the referenced value to append
     * @param boolean $merge   flag if array elements shall be merged
     *
     * @return Data
     */
	public function appendByRef($tpl_var, &$value, $merge = false)
	{
		if ($tpl_var !== '' && isset($value)) {
			if (!isset($this->tpl_vars[ $tpl_var ])) {
				$this->tpl_vars[ $tpl_var ] = new \Smarty\Variable();
			}
			if (!is_array($this->tpl_vars[ $tpl_var ]->value)) {
				settype($this->tpl_vars[ $tpl_var ]->value, 'array');
			}
			if ($merge && is_array($value)) {
				foreach ($value as $_key => $_val) {
					$this->tpl_vars[ $tpl_var ]->value[ $_key ] = &$value[ $_key ];
				}
			} else {
				$this->tpl_vars[ $tpl_var ]->value[] = &$value;
			}
			$this->_updateScope($tpl_var);
		}
		return $this;
	}

    /**
     * assigns values to template variables by reference
     *
     * @param string  $tpl_var the template variable name
     * @param         $value
     * @param boolean $nocache if true any output of this variable will be not cached
     *
     * @return Data
     */
	public function assignByRef($tpl_var, &$value, $nocache)
	{
		if ($tpl_var !== '') {
			$this->tpl_vars[ $tpl_var ] = new \Smarty\Variable(null, $nocache);
			$this->tpl_vars[ $tpl_var ]->value = &$value;
			$this->_updateScope($tpl_var);
		}
		return $this;
	}

	protected function _updateScope($varName, $tagScope = 0) {
		// implemented in \Smarty\Template only
	}

    /**
     * Returns a single or all template variables
     *
     * @param string                                                  $varName       variable name or null
     * @param bool $searchParents include parent templates?
     *
     * @return mixed variable value or or array of variables
     *@api  Smarty::getTemplateVars()
     * @link https://www.smarty.net/docs/en/api.get.template.vars.tpl
     *
     */
    public function getTemplateVars($varName = null, Data $_ptr = null, $searchParents = true)
    {
	    if (isset($varName)) {
		    $_var = $this->_getVariable($varName, $_ptr, $searchParents, false);
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
		    if ($searchParents && isset(Smarty::$global_tpl_vars)) {
			    foreach (Smarty::$global_tpl_vars as $key => $var) {
				    if (!array_key_exists($key, $_result)) {
					    $_result[ $key ] = $var->value;
				    }
			    }
		    }
		    return $_result;
	    }
    }

	/**
	 * gets the object of a Smarty variable
	 *
	 * @param string $varName the name of the Smarty variable
	 * @param Data|null $_ptr optional pointer to data object
	 * @param bool $searchParents search also in parent data
	 * @param bool $errorEnable
	 *
	 * @return Variable
	 */
	public function _getVariable(
		$varName,
		Data $_ptr = null,
		$searchParents = true,
		$errorEnable = true
	) {
		if ($_ptr === null) {
			$_ptr = $this;
		}
		while ($_ptr !== null) {
			if (isset($_ptr->tpl_vars[ $varName ])) {
				// found it, return it
				return $_ptr->tpl_vars[ $varName ];
			}
			// not found, try at parent
			if ($searchParents && isset($_ptr->parent)) {
				$_ptr = $_ptr->parent;
			} else {
				$_ptr = null;
			}
		}
		if (isset(Smarty::$global_tpl_vars[ $varName ])) {
			// found it, return it
			return Smarty::$global_tpl_vars[ $varName ];
		}
		if ($errorEnable && $this->_getSmartyObj()->error_unassigned) {
			// force a notice
			$x = $$varName;
		}
		return new UndefinedVariable;
	}

    /**
     * Follow the parent chain an merge template and config variables
     *
     * @param Data|null $data
     */
    public function _mergeVars(Data $data = null)
    {
        if (isset($data)) {
            if (!empty($this->tpl_vars)) {
                $data->tpl_vars = array_merge($this->tpl_vars, $data->tpl_vars);
            }
            if (!empty($this->config_vars)) {
                $data->config_vars = array_merge($this->config_vars, $data->config_vars);
            }
        } else {
            $data = $this;
        }
        if (isset($this->parent)) {
            $this->parent->_mergeVars($data);
        }
    }

    /**
     * Return true if this instance is a Data obj
     *
     * @return bool
     */
    public function _isDataObj()
    {
        return $this->_objType === 4;
    }

    /**
     * Return true if this instance is a template obj
     *
     * @return bool
     */
    public function _isTplObj()
    {
        return $this->_objType === 2;
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
	 * load a config file, optionally load just selected sections
	 *
	 * @param string                                                  $config_file filename
	 * @param mixed                                                   $sections    array of section names, single
	 *                                                                             section or null
	 *
	 * @return Data
	 * @throws \Exception
	 *@api  Smarty::configLoad()
	 * @link https://www.smarty.net/docs/en/api.config.load.tpl
	 *
	 */
	public function configLoad($config_file, $sections = null)
	{
		$this->_loadConfigFile($this, $config_file, $sections, null);
		return $this;
	}

	/**
	 * load a config file, optionally load just selected sections
	 *
	 * @param string $config_file filename
	 * @param mixed                                                   $sections    array of section names, single
	 *                                                                             section or null
	 * @param int                                                     $scope       scope into which config variables
	 *                                                                             shall be loaded
	 *
	 * @throws \Exception
	 *@link https://www.smarty.net/docs/en/api.config.load.tpl
	 *
	 * @api  Smarty::configLoad()
	 */
	public function _loadConfigFile($config_file, $sections = null, $scope = 0)
	{
		/* @var \Smarty $smarty */
		$smarty = $this->_getSmartyObj();
		/* @var \Smarty\Template $confObj */
		$confObj = new Template($config_file, $smarty, $this, null, null, null, null, true);
		$confObj->caching = Smarty::CACHING_OFF;
		$confObj->source->config_sections = $sections;
		$confObj->source->scope = $scope;
		$confObj->compiled = \Smarty\Template\Compiled::load($confObj);
		$confObj->compiled->render($confObj);
		if ($this->_isTplObj()) {
			$this->compiled->file_dependency[ $confObj->source->uid ] =
				array($confObj->source->filepath, $confObj->source->getTimeStamp(), $confObj->source->type);
		}
	}

	/**
	 * gets  a config variable value
	 *
	 * @param string                                                  $varName the name of the config variable
	 * @param bool                                                    $errorEnable
	 *
	 * @return null|string  the value of the config variable
	 */
	public function getConfigVariable($varName = null, $errorEnable = true)
	{
		$_ptr = $this;
		while ($_ptr !== null) {
			if (isset($_ptr->config_vars[ $varName ])) {
				// found it, return it
				return $_ptr->config_vars[ $varName ];
			}
			// not found, try at parent
			$_ptr = $_ptr->parent;
		}
		if ($this->smarty->error_unassigned && $errorEnable) {
			// force a notice
			$x = $$varName;
		}
		return null;
	}

	/**
	 * Returns a single or all config variables
	 *
	 * @api  Smarty::getConfigVars()
	 * @link https://www.smarty.net/docs/en/api.get.config.vars.tpl
	 *
	 * @param string                                                  $varname        variable name or null
	 * @param bool                                                    $search_parents include parent templates?
	 *
	 * @return mixed variable value or or array of variables
	 */
	public function getConfigVars($varname = null, $search_parents = true)
	{
		$_ptr = $this;
		$var_array = array();
		while ($_ptr !== null) {
			if (isset($varname)) {
				if (isset($_ptr->config_vars[ $varname ])) {
					return $_ptr->config_vars[ $varname ];
				}
			} else {
				$var_array = array_merge($_ptr->config_vars, $var_array);
			}
			// not found, try at parent
			if ($search_parents) {
				$_ptr = $_ptr->parent;
			} else {
				$_ptr = null;
			}
		}
		if (isset($varname)) {
			return '';
		} else {
			return $var_array;
		}
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
			if (isset(Smarty::$global_tpl_vars[ $varName ])) {
				return Smarty::$global_tpl_vars[ $varName ]->value;
			} else {
				return '';
			}
		} else {
			$_result = array();
			foreach (Smarty::$global_tpl_vars as $key => $var) {
				$_result[ $key ] = $var->value;
			}
			return $_result;
		}
	}

	/**
	 * gets  a stream variable
	 *
	 * @param string                                                  $variable the stream of the variable
	 *
	 * @return mixed
	 * @throws \Smarty\Exception
	 *@api Smarty::getStreamVariable()
	 *
	 */
	public function getStreamVariable($variable)
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
		$smarty = $this->smarty ?? $this;
		if ($smarty->error_unassigned) {
			throw new Exception('Undefined stream variable "' . $variable . '"');
		} else {
			return null;
		}
	}

}

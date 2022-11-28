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
 *
 * @package    Smarty
 * @subpackage Template
 *
 * @property int    $scope
 * @property Smarty $smarty
 * The following methods will be dynamically loaded by the extension handler when they are called.
 * They are located in a corresponding Smarty_Internal_Method_xxxx class
 *
 * @method mixed _getConfigVariable(string $varName, bool $errorEnable = true)
 * @method mixed getConfigVariable(string $varName, bool $errorEnable = true)
 * @method mixed getConfigVars(string $varName = null, bool $searchParents = true)
 * @method mixed getGlobal(string $varName = null)
 * @method mixed getStreamVariable(string $variable)
 * @method Data clearAssign(mixed $tpl_var)
 * @method Data clearAllAssign()
 * @method Data clearConfig(string $varName = null)
 * @method Data configLoad(string $config_file, mixed $sections = null, string $scope = 'local')
 */
abstract class Data
{
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
    public $template_class = 'Smarty_Internal_Template';

    /**
     * template variables
     *
     * @var Variable[]
     */
    public $tpl_vars = array();

    /**
     * parent template (if any)
     *
     * @var Smarty|Smarty_Internal_Template|Smarty_Data
     */
    public $parent = null;

    /**
     * configuration settings
     *
     * @var string[]
     */
    public $config_vars = array();

    /**
     * extension handler
     *
     * @var Smarty_Internal_Extension_Handler
     */
    public $ext = null;

    /**
     * \Smarty\Data constructor.
     *
     * Install extension handler
     */
    public function __construct()
    {
        $this->ext = new Smarty_Internal_Extension_Handler();
        $this->ext->objType = $this->_objType;
    }

    /**
     * assigns a Smarty variable
     *
     * @param array|string $tpl_var the template variable name(s)
     * @param mixed        $value   the value to assign
     * @param boolean      $nocache if true any output of this variable will be not cached
     *
     * @return Data current Data (or Smarty or Smarty_Internal_Template) instance for
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
                     * @var Smarty_Internal_Template $this
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
     * @return Data|\Smarty_Internal_Template|\Smarty
     *@link https://www.smarty.net/docs/en/api.append.tpl
     *
     * @api  Smarty::append()
     */
    public function append($tpl_var, $value = null, $merge = false, $nocache = false)
    {
        return $this->ext->append->append($this, $tpl_var, $value, $merge, $nocache);
    }

    /**
     * assigns a global Smarty variable
     *
     * @param string  $varName the global variable name
     * @param mixed   $value   the value to assign
     * @param boolean $nocache if true any output of this variable will be not cached
     *
     * @return Data|\Smarty_Internal_Template|\Smarty
     */
    public function assignGlobal($varName, $value = null, $nocache = false)
    {
        return $this->ext->assignGlobal->assignGlobal($this, $varName, $value, $nocache);
    }

    /**
     * appends values to template variables by reference
     *
     * @param string  $tpl_var the template variable name
     * @param mixed   &$value  the referenced value to append
     * @param boolean $merge   flag if array elements shall be merged
     *
     * @return Data|\Smarty_Internal_Template|\Smarty
     */
    public function appendByRef($tpl_var, &$value, $merge = false)
    {
        return $this->ext->appendByRef->appendByRef($this, $tpl_var, $value, $merge);
    }

    /**
     * assigns values to template variables by reference
     *
     * @param string  $tpl_var the template variable name
     * @param         $value
     * @param boolean $nocache if true any output of this variable will be not cached
     *
     * @return Data|\Smarty_Internal_Template|\Smarty
     */
    public function assignByRef($tpl_var, &$value, $nocache = false)
    {
        return $this->ext->assignByRef->assignByRef($this, $tpl_var, $value, $nocache);
    }

    /**
     * Returns a single or all template variables
     *
     * @param string                                                  $varName       variable name or null
     * @param Data|\Smarty_Internal_Template|\Smarty $_ptr          optional pointer to data object
     * @param bool $searchParents include parent templates?
     *
     * @return mixed variable value or or array of variables
     *@api  Smarty::getTemplateVars()
     * @link https://www.smarty.net/docs/en/api.get.template.vars.tpl
     *
     */
    public function getTemplateVars($varName = null, Data $_ptr = null, $searchParents = true)
    {
        return $this->ext->getTemplateVars->getTemplateVars($this, $varName, $_ptr, $searchParents);
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
     * Handle unknown class methods
     *
     * @param string $name unknown method-name
     * @param array  $args argument array
     *
     * @return mixed
     */
    public function __call($name, $args)
    {
        return $this->ext->_callExternalMethod($this, $name, $args);
    }
}

<?php

/**
 * Smarty Method ConfigLoad
 *
 * Smarty::configLoad() method
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Method_ConfigLoad
{
    /**
     * Valid for all objects
     *
     * @var int
     */
    public $objMap = 7;

    /**
     * load a config file, optionally load just selected sections
     *
     * @api  Smarty::configLoad()
     * @link http://www.smarty.net/docs/en/api.config.load.tpl
     *
     * @param \Smarty_Internal_Data|\Smarty_Internal_Template|\Smarty $data
     * @param  string                                                 $config_file filename
     * @param  mixed                                                  $sections    array of section names, single
     *                                                                             section or null
     *
     * @return \Smarty|\Smarty_Internal_Data|\Smarty_Internal_Template
     * @throws \SmartyException
     */
    public function configLoad(Smarty_Internal_Data $data, $config_file, $sections = null)
    {
        $this->_loadConfigFile($data, $config_file, $sections, null);
        return $data;
    }

    /**
     * load a config file, optionally load just selected sections
     *
     * @api  Smarty::configLoad()
     * @link http://www.smarty.net/docs/en/api.config.load.tpl
     *
     * @param \Smarty|\Smarty_Internal_Data|\Smarty_Internal_Template $data
     * @param  string                                                 $config_file filename
     * @param  mixed                                                  $sections    array of section names, single
     *                                                                             section or null
     * @param int                                                     $scope       scope into which config variables
     *                                                                             shall be loaded
     *
     * @return \Smarty|\Smarty_Internal_Data|\Smarty_Internal_Template
     * @throws \SmartyException
     */
    public function _loadConfigFile(Smarty_Internal_Data $data, $config_file, $sections = null, $scope = 0)
    {
        /* @var \Smarty $smarty */
        $smarty = isset($data->smarty) ? $data->smarty : $data;
        /* @var \Smarty_Internal_Template $confObj */
        $confObj = new Smarty_Internal_Template($config_file, $smarty, $data);
        $confObj->caching = Smarty::CACHING_OFF;
        $confObj->source = Smarty_Template_Config::load($confObj);
        $confObj->source->config_sections = $sections;
        $confObj->source->scope = $scope;
        $confObj->compiled = Smarty_Template_Compiled::load($confObj);
        $confObj->compiled->render($confObj);
        if ($data->_objType == 2) {
            $data->compiled->file_dependency[ $confObj->source->uid ] =
                array($confObj->source->filepath, $confObj->source->getTimeStamp(), $confObj->source->type);
        }
    }

    /**
     * load config variables into template object
     *
     * @param \Smarty_Internal_Template $tpl
     * @param  array                    $_config_vars
     *
     */
    public function _loadConfigVars(Smarty_Internal_Template $tpl, $_config_vars)
    {
        $this->_assignConfigVars($tpl->parent, $tpl, $_config_vars);
        if ($tpl->parent->_objType == 2 && ($tpl->source->scope || $tpl->parent->scope)) {
            $scope = $tpl->source->scope | $tpl->scope;
            if ($scope) {
                // update scopes
                foreach ($tpl->smarty->ext->_updateScope->_getAffectedScopes($tpl->parent, $scope) as $ptr) {
                    $this->_assignConfigVars($ptr, $tpl, $_config_vars);
                }
                if ($scope & Smarty::SCOPE_LOCAL) {
                    //$this->_updateVarStack($tpl, $varName);
                }
            }
        }
    }

    /**
     * Assign all config variables in given scope
     *
     * @param \Smarty_Internal_Data     $scope_ptr
     * @param \Smarty_Internal_Template $tpl
     * @param  array                    $_config_vars
     */
    public function _assignConfigVars(Smarty_Internal_Data $scope_ptr, Smarty_Internal_Template $tpl, $_config_vars)
    {
        // copy global config vars
        foreach ($_config_vars[ 'vars' ] as $variable => $value) {
            if ($tpl->smarty->config_overwrite || !isset($scope_ptr->config_vars[ $variable ])) {
                $scope_ptr->config_vars[ $variable ] = $value;
            } else {
                $scope_ptr->config_vars[ $variable ] =
                    array_merge((array) $scope_ptr->config_vars[ $variable ], (array) $value);
            }
        }
        // scan sections
        $sections = $tpl->source->config_sections;
        if (!empty($sections)) {
            foreach ((array) $sections as $tpl_section) {
                if (isset($_config_vars[ 'sections' ][ $tpl_section ])) {
                    foreach ($_config_vars[ 'sections' ][ $tpl_section ][ 'vars' ] as $variable => $value) {
                        if ($tpl->smarty->config_overwrite || !isset($scope_ptr->config_vars[ $variable ])) {
                            $scope_ptr->config_vars[ $variable ] = $value;
                        } else {
                            $scope_ptr->config_vars[ $variable ] =
                                array_merge((array) $scope_ptr->config_vars[ $variable ], (array) $value);
                        }
                    }
                }
            }
        }
    }

    /**
     * gets  a config variable value
     *
     * @param \Smarty_Internal_Template $tpl     template object
     * @param string                    $varName the name of the config variable
     * @param bool                      $errorEnable
     *
     * @return mixed  the value of the config variable
     */
    public function _getConfigVariable(Smarty_Internal_Template $tpl, $varName, $errorEnable = true)
    {
        $_ptr = $tpl;
        while ($_ptr !== null) {
            if (isset($_ptr->config_vars[ $varName ])) {
                // found it, return it
                return $_ptr->config_vars[ $varName ];
            }
            // not found, try at parent
            $_ptr = $_ptr->parent;
        }
        if ($tpl->smarty->error_unassigned && $errorEnable) {
            // force a notice
            $x = $$varName;
        }
        return null;
    }
}

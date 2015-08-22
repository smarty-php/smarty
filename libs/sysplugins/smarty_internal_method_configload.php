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
     * @param string                                                  $scope       scope into which config variables
     *                                                                             shall be loaded
     *
     * @return \Smarty|\Smarty_Internal_Data|\Smarty_Internal_Template
     * @throws \SmartyException
     */
    public function configLoad(Smarty_Internal_Data $data, $config_file, $sections = null, $scope = 'local')
    {
        /* @var \Smarty $smarty */
        $smarty = isset($data->smarty) ? $data->smarty : $data;
        /* @var \Smarty_Internal_Template $confObj */
        $confObj = new $smarty->template_class($config_file, $smarty, $data);
        $confObj->caching = Smarty::CACHING_OFF;
        $confObj->source = Smarty_Template_Config::load($confObj);
        $confObj->source->config_sections = $sections;
        $confObj->source->scope = $scope;
        $confObj->compiled = Smarty_Template_Compiled::load($confObj);
        $confObj->compiled->render($confObj);
        if ($data->_objType == 2) {
            $data->compiled->file_dependency[$confObj->source->uid] = array($confObj->source->filepath,
                                                                            $confObj->source->getTimeStamp(),
                                                                            $confObj->source->type);
        }
        return $data;
    }

    /**
     * load config variables into template object
     *
     * @param \Smarty_Internal_Template $_template
     * @param  array                    $_config_vars
     */
    static function _loadConfigVars(Smarty_Internal_Template $_template, $_config_vars)
    {
        $scope = $_template->source->scope;
        // pointer to scope (local scope is parent of template object
        $scope_ptr = $_template->parent;
        if ($scope == 'parent') {
            if (isset($_template->parent->parent)) {
                $scope_ptr = $_template->parent->parent;
            }
        } elseif ($scope == 'root' || $scope == 'global') {
            while (isset($scope_ptr->parent)) {
                $scope_ptr = $scope_ptr->parent;
            }
        }
        // copy global config vars
        foreach ($_config_vars['vars'] as $variable => $value) {
            if ($_template->smarty->config_overwrite || !isset($scope_ptr->config_vars[$variable])) {
                $scope_ptr->config_vars[$variable] = $value;
            } else {
                $scope_ptr->config_vars[$variable] = array_merge((array) $scope_ptr->config_vars[$variable], (array) $value);
            }
        }
        // scan sections
        $sections = $_template->source->config_sections;
        if (!empty($sections)) {
            foreach ((array) $sections as $_template_section) {
                if (isset($_config_vars['sections'][$_template_section])) {
                    foreach ($_config_vars['sections'][$_template_section]['vars'] as $variable => $value) {
                        if ($_template->smarty->config_overwrite || !isset($scope_ptr->config_vars[$variable])) {
                            $scope_ptr->config_vars[$variable] = $value;
                        } else {
                            $scope_ptr->config_vars[$variable] = array_merge((array) $scope_ptr->config_vars[$variable], (array) $value);
                        }
                    }
                }
            }
        }
    }
}

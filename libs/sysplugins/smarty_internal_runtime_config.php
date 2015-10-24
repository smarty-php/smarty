<?php

/**
 * Runtime Methods _getConfigVariable
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Config
{
    /**
     * gets  a config variable value
     *
     * @param \Smarty_Internal_Template $tpl     template object
     * @param string $varName the name of the config variable
     * @param bool    $errorEnable
     *
     * @return mixed  the value of the config variable
     */
    public function _getConfigVariable(\Smarty_Internal_Template $tpl, $varName, $errorEnable = true)
    {
        $_ptr = $tpl;
        while ($_ptr !== null) {
            if (isset($_ptr->config_vars[$varName])) {
                // found it, return it
                return $_ptr->config_vars[$varName];
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

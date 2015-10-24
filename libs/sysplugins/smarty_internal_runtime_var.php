<?php

/**
 * Runtime Methods createLocalArrayVariable
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Var
{
    /**
     * Template code runtime function to create a local Smarty variable for array assignments
     *
     * @param \Smarty_Internal_Template $tpl     template object
     * @param string                    $varName template variable name
     * @param bool                      $nocache cache mode of variable
     * @param int                       $scope   scope of variable
     */
    public function createLocalArrayVariable(\Smarty_Internal_Template $tpl, $varName, $nocache = false,
                                             $scope = Smarty::SCOPE_LOCAL)
    {
        if (!isset($tpl->tpl_vars[$varName])) {
            $tpl->tpl_vars[$varName] = new Smarty_Variable(array(), $nocache, $scope);
        } else {
            $tpl->tpl_vars[$varName] = clone $tpl->tpl_vars[$varName];
            if ($scope != Smarty::SCOPE_LOCAL) {
                $tpl->tpl_vars[$varName]->scope = $scope;
            }
            if (!(is_array($tpl->tpl_vars[$varName]->value) ||
                $tpl->tpl_vars[$varName]->value instanceof ArrayAccess)
            ) {
                settype($tpl->tpl_vars[$varName]->value, 'array');
            }
        }
    }
}

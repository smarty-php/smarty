<?php

/**
 * Runtime Extension updateScope
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_UpdateScope
{
    /**
     * Update new assigned template variable in other effected scopes
     *
     * @param \Smarty_Internal_Data $tpl     data object
     * @param string                $varName variable name
     * @param int                   $scope   scope to which bubble up variable value
     */
    public function updateScope(Smarty_Internal_Data $tpl, $varName, $scope = Smarty::SCOPE_LOCAL)
    {
        $scopes = array();
        if ($scope) {
            $scopes[] = $scope;
        }
        if ($tpl->scope) {
            $scopes[] = $tpl->scope;
        }
        if (empty($scopes)) {
            return;
        }
        /* @var Smarty_Internal_Data $ptr */
        $ptr = null;
        foreach ($scopes as $s) {
            $s = ($bubble_up = $s >= Smarty::SCOPE_BUBBLE_UP) ? $s - Smarty::SCOPE_BUBBLE_UP : $s;
            if ($bubble_up && $s) {
                $oldGlobal = null;
                if ($s == Smarty::SCOPE_GLOBAL) {
                    $oldGlobal =
                        isset(Smarty::$global_tpl_vars[ $varName ]) ? Smarty::$global_tpl_vars[ $varName ] : null;
                    Smarty::$global_tpl_vars[ $varName ] = $tpl->tpl_vars[ $varName ];
                }
                if (isset($tpl->parent) && $tpl->parent->_objType == 2) {
                    $ptr = $tpl->parent;
                    if ($s == Smarty::SCOPE_GLOBAL && isset($oldGlobal) && isset($ptr->tpl_vars[ $varName ]) &&
                        $ptr->tpl_vars[ $varName ] !== $oldGlobal
                    ) {
                        continue;
                    }
                    $ptr->tpl_vars[ $varName ] = $tpl->tpl_vars[ $varName ];
                    if (isset($ptr->parent)) {
                        $ptr = $ptr->parent;
                    }
                } elseif (isset($tpl->parent)) {
                    $ptr = $tpl->parent;
                }
                if ($s == Smarty::SCOPE_PARENT) {
                    continue;
                }
                while (isset($ptr) && $ptr->_objType == 2) {
                    $ptr->tpl_vars[ $varName ] = $tpl->tpl_vars[ $varName ];
                    $ptr = $ptr->parent;
                }
                if ($s == Smarty::SCOPE_TPL_ROOT) {
                    continue;
                }
                while (isset($ptr) && $s != Smarty::SCOPE_GLOBAL) {
                    $ptr->tpl_vars[ $varName ] = $tpl->tpl_vars[ $varName ];
                    $ptr = isset($ptr->parent) ? $ptr->parent : null;
                }
            }
        }
    }
}

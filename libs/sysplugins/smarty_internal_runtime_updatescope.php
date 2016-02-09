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
     * Update new assigned template or config variable in other effected scopes
     *
     * @param Smarty_Internal_Template $tpl     data object
     * @param string|null              $varName variable name
     * @param int                      $scope   scope to which bubble up variable value
     *
     */
    public function _updateScope(Smarty_Internal_Template $tpl, $varName, $scope = 0)
    {
        $scope = $scope | $tpl->scope;
        if ($scope) {
            if ($scope & Smarty::SCOPE_GLOBAL && $varName) {
                Smarty::$global_tpl_vars[ $varName ] = $tpl->tpl_vars[ $varName ];
            }
            // update scopes
            foreach ($this->_getAffectedScopes($tpl, $scope) as $ptr) {
                $this->_updateObject($ptr, $tpl, $varName);
            }
            if ($scope & Smarty::SCOPE_LOCAL) {
                $this->_updateVarStack($tpl, $varName);
            }
        }
    }

    /**
     * Get array of objects which needs to be updated  by given scope value
     *
     * @param Smarty_Internal_Template $tpl
     * @param int                      $scope scope to which bubble up variable value
     *
     * @return array
     */
    public function _getAffectedScopes(Smarty_Internal_Template $tpl, $scope)
    {
        $_stack = array();
        $ptr = $tpl->parent;
        while (isset($ptr) && $ptr->_objType == 2) {
            if (($scope & Smarty::SCOPE_BUBBLE_UP) || ($scope & Smarty::SCOPE_PARENT)) {
                $_stack[] = $ptr;
                $scope = $scope & ~Smarty::SCOPE_PARENT;
            } elseif (($scope & Smarty::SCOPE_TPL_ROOT) && (!isset($ptr->parent) || $ptr->parent->_objType != 2)) {
                $_stack[] = $ptr;
            }
            $ptr = $ptr->parent;
        }
        if ($scope & Smarty::SCOPE_SMARTY) {
            if (isset($tpl->smarty)) {
                $_stack[] = $tpl->smarty;
            }
        } elseif ($scope & Smarty::SCOPE_ROOT) {
            while (isset($ptr)) {
                if ($ptr->_objType != 2) {
                    $_stack[] = $ptr;
                    break;
                }
                $ptr = $ptr->parent;
            }
        }
        return $_stack;
    }

    /**
     * Update variable in object
     *
     * @param \Smarty_Internal_Data     $to
     * @param \Smarty_Internal_Template $from
     * @param string|null               $varName variable name
     */
    public function _updateObject(Smarty_Internal_Data $to, Smarty_Internal_Template $from, $varName)
    {
        if (!isset($to->tpl_vars[ $varName ])) {
            $to->tpl_vars[ $varName ] = clone $from->tpl_vars[ $varName ];
        } else {
            $to->tpl_vars[ $varName ] = clone $to->tpl_vars[ $varName ];
            $to->tpl_vars[ $varName ]->value = $from->tpl_vars[ $varName ]->value;
        }
        if ($to->_objType == 2) {
            $this->_updateVarStack($to, $varName);
        }
    }

    /**
     * Update variable in template local variable stack
     *
     * @param \Smarty_Internal_Template $tpl
     * @param string|null               $varName variable name or null for config variables
     */
    public function _updateVarStack(Smarty_Internal_Template $tpl, $varName)
    {
        $i = 0;
        while (isset($tpl->_cache[ 'varStack' ][ $i ])) {
            $tpl->_cache[ 'varStack' ][ $i ][ 'tpl' ][ $varName ] = $tpl->tpl_vars[ $varName ];
            $i ++;
        }
    }
}

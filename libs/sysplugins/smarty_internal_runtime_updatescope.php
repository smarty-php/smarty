<?php

/**
 * Runtime Extension updateScope
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 **/
class Smarty_Internal_Runtime_UpdateScope
{
    /**
     * Update new assigned template or config variable in other effected scopes
     *
     * @param Smarty_Internal_Template $tpl      data object
     * @param string|null              $varName  variable name
     * @param int                      $tagScope tag scope to which bubble up variable value
     */
    public function _updateScope(Smarty_Internal_Template $tpl, $varName, $tagScope = 0)
    {
        if ($tagScope) {
            $this->_updateVarStack($tpl, $varName);
            $tagScope = $tagScope & ~Smarty::SCOPE_LOCAL;
            if (!$tpl->scope && !$tagScope) {
                return;
            }
        }
        $mergedScope = $tagScope | $tpl->scope;
        if ($mergedScope) {
            if ($mergedScope & Smarty::SCOPE_GLOBAL && $varName) {
                Smarty::$global_tpl_vars[ $varName ] = $tpl->tpl_vars[ $varName ];
            }
            // update scopes
            foreach ($this->_getAffectedScopes($tpl, $tagScope) as $ptr) {
                $this->_updateVariableInOtherScope($ptr->tpl_vars, $tpl, $varName);
                if ($tagScope && $ptr->_isTplObj() && isset($tpl->_cache[ 'varStack' ])) {
                    $this->_updateVarStack($ptr, $varName);
                }
            }
        }
    }

    /**
     * Get array of objects which needs to be updated  by given scope value
     *
     * @param Smarty_Internal_Template $tpl
     * @param int                      $tagScope tag scope to which bubble up variable value
     *
     * @return array
     */
    public function _getAffectedScopes(Smarty_Internal_Template $tpl, $tagScope)
    {
        $mergedScope = $tagScope | $tpl->scope;
        $_stack = array();
        $ptr = $tpl->parent;
        if ($mergedScope && isset($ptr) && $ptr->_isTplObj()) {
            $_stack[] = $ptr;
            if ($tpl->inheritance && $tagScope & Smarty::SCOPE_PARENT) {
                $inheritanceRoot = $tpl;
                while ($inheritanceRoot->inheritance && $inheritanceRoot->parent
                    && $inheritanceRoot->parent->_isTplObj()
                    && $inheritanceRoot->parent->inheritance === $inheritanceRoot->inheritance
                ) {
                    $inheritanceRoot = $inheritanceRoot->parent;
                }
                if ($inheritanceRoot->parent && $inheritanceRoot->parent !== $ptr) {
                    $_stack[] = $inheritanceRoot->parent;
                }
            }
            $mergedScope = $mergedScope & ~Smarty::SCOPE_PARENT;
            if (!$mergedScope) {
                // only parent was set, we are done
                return $_stack;
            }
            $ptr = $ptr->parent;
        }
        while (isset($ptr) && $ptr->_isTplObj()) {
            $_stack[] = $ptr;
            $ptr = $ptr->parent;
        }
        if ($mergedScope & Smarty::SCOPE_SMARTY) {
            if (isset($tpl->smarty)) {
                $_stack[] = $tpl->smarty;
            }
        } elseif ($mergedScope & Smarty::SCOPE_ROOT) {
            while (isset($ptr)) {
                if (!$ptr->_isTplObj()) {
                    $_stack[] = $ptr;
                    break;
                }
                $ptr = $ptr->parent;
            }
        }
        return $_stack;
    }

    /**
     * Update variable in other scope
     *
     * @param array                     $tpl_vars template variable array
     * @param \Smarty_Internal_Template $from
     * @param string                    $varName  variable name
     */
    public function _updateVariableInOtherScope(&$tpl_vars, Smarty_Internal_Template $from, $varName)
    {
        if (!isset($tpl_vars[ $varName ])) {
            $tpl_vars[ $varName ] = clone $from->tpl_vars[ $varName ];
        } else {
            $tpl_vars[ $varName ] = clone $tpl_vars[ $varName ];
            $tpl_vars[ $varName ]->value = $from->tpl_vars[ $varName ]->value;
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
            $this->_updateVariableInOtherScope($tpl->_cache[ 'varStack' ][ $i ][ 'tpl' ], $tpl, $varName);
            $i++;
        }
    }
}

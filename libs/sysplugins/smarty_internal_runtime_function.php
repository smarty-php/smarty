<?php

/**
 * Runtime Method _callTemplateFunction
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Function
{
    /**
     * Call template function
     *
     * @param string                           $name        template function name
     * @param object|\Smarty_Internal_Template $_smarty_tpl template object
     * @param array                            $params      parameter array
     * @param bool                             $nocache     true if called nocache
     *
     * @throws \SmartyException
     */
    public function callTemplateFunction($name, Smarty_Internal_Template $_smarty_tpl, $params, $nocache)
    {
        if (isset($_smarty_tpl->tpl_function[$name])) {
            if (!$_smarty_tpl->caching || ($_smarty_tpl->caching && $nocache)) {
                $function = $_smarty_tpl->tpl_function[$name]['call_name'];
            } else {
                if (isset($_smarty_tpl->tpl_function[$name]['call_name_caching'])) {
                    $function = $_smarty_tpl->tpl_function[$name]['call_name_caching'];
                } else {
                    $function = $_smarty_tpl->tpl_function[$name]['call_name'];
                }
            }
            if (function_exists($function)) {
                $function ($_smarty_tpl, $params);
                return;
            }
            // try to load template function dynamically
            if (Smarty_Internal_Function_Call_Handler::call($name, $_smarty_tpl, $function)) {
                $function ($_smarty_tpl, $params);
                return;
            }
        }
        throw new SmartyException("Unable to find template function '{$name}'");
    }

}
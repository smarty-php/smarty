<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     assign_debug_info
 * Purpose:  assign debug info to the template
 * -------------------------------------------------------------
 */
function smarty_function_assign_debug_info($args, &$smarty_obj)
{
    $assigned_vars = $smarty_obj->_tpl_vars;
    ksort($assigned_vars);
    if (is_array($smarty_obj->_config[0])) {
        $config_vars = $smarty_obj->_config[0];
        ksort($config_vars);
        $smarty_obj->assign("_debug_config_keys", array_keys($config_vars));
        $smarty_obj->assign("_debug_config_vals", array_values($config_vars));
    }   
    
    $included_templates = $smarty_obj->_smarty_debug_info;
    
    $smarty_obj->assign("_debug_keys", array_keys($assigned_vars));
    $smarty_obj->assign("_debug_vals", array_values($assigned_vars));
    
    $smarty_obj->assign("_debug_tpls", $included_templates);
}

/* vim: set expandtab: */

?>

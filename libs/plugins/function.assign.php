<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     assign
 * Purpose:  assign a value to a template variable
 * -------------------------------------------------------------
 */
function smarty_function_assign($args, &$smarty_obj)
{
    extract($args);

    if (empty($var)) {
        $smarty_obj->_trigger_error_msg("assign: missing 'var' parameter");
        return;
    }

    if (!in_array('value', array_keys($args))) {
        $smarty_obj->_trigger_error_msg("assign: missing 'value' parameter");
        return;
    }

    $smarty_obj->assign($var, $value);
    return true;
}

/* vim: set expandtab: */

?>

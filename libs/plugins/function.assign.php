<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins


/**
 * Smarty {assign} function plugin
 *
 * Type:     function<br>
 * Name:     assign<br>
 * Purpose:  assign a value to a template variable
 * @link http://smarty.php.net/manual/en/language.custom.functions.php#LANGUAGE.FUNCTION.ASSIGN {assign}
 *       (Smarty online manual)
 * @param array Format: array('var' => variable name, 'value' => value to assign)
 * @param Smarty
 */
function smarty_function_assign($params, &$smarty)
{
    if (empty($params['var'])) {
        $smarty->trigger_error("assign: missing 'var' parameter");
        return;
    }

    if (!in_array('value', array_keys($params))) {
        $smarty->trigger_error("assign: missing 'value' parameter");
        return;
    }

    $smarty->assign($params['var'], $params['value']);
}

/* vim: set expandtab: */

?>

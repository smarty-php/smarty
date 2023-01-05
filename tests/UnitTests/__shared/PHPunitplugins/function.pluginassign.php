<?php
/**
 * Smarty plugin for assign
 *


 */

/**
 * Smarty {pluginassign}
 *
 * @param array  $params   parameter array
 * @param object $template template object
 *
 * @return string
 */
function smarty_function_pluginassign($params, $template)
{
    $template->assign($params[ 'var' ], $params[ 'value' ]);
    return '';
}

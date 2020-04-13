<?php
/**
 * Smarty plugin params
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

/**
 * Smarty {gatparams}
 *
 * @param array  $params   parameter array
 * @param object $template template object
 *
 * @return string
 */
function smarty_function_getparams($params, Smarty_Internal_Template $template)
{
    return var_export($params, true);
}

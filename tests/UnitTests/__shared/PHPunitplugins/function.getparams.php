<?php
/**
 * Smarty plugin params
 *


 */

use Smarty\Template;

/**
 * Smarty {gatparams}
 *
 * @param array  $params   parameter array
 * @param object $template template object
 *
 * @return string
 */
function smarty_function_getparams($params, Template $template)
{
    return var_export($params, true);
}

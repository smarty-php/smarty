<?php
/**
 * Smarty plugin
 *
 * @package    Smarty
 * @subpackage PluginsFilter
 */

use Smarty\Template;

/**
 * Smarty htmlspecialchars variablefilter plugin
 *
 * @param string                    $source input string
 * @param \Smarty\Template $template
 *
 * @return string filtered output
 */
function smarty_variablefilter_htmlspecialchars($source, Template $template)
{
    return htmlspecialchars($source, ENT_QUOTES, \Smarty\Smarty::$_CHARSET);
}

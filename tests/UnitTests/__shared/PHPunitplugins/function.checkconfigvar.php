<?php
/**
 * Smarty plugin for testing scopes in config vars
 *


 */

use Smarty\Template;

/**
 * Smarty {checkconfigvar}
 *
 * @param array $params parameter array
 * @param object $template template object
 *
 * @return string
 */
function smarty_function_checkconfigvar($params, $template)
{
    $output = '';
    $types = array('template', 'data', 'global');
    if (isset($params['types'])) {
        $types = (array)$params['types'];
    }
    $var = $params['var'];
    $ptr = $template;
    while ($ptr) {
        if (in_array('template', $types) && $ptr instanceof Template) {
            $output .= "#{$ptr->getSource()->name}:\${$var} =";
            $output .= $ptr->hasConfigVariable($var) ? preg_replace('/\s/', '', var_export($ptr->getConfigVariable($var), true)) : 'null';
            $ptr = $ptr->parent;
        } elseif (in_array('data', $types) && !($ptr instanceof Template || $ptr instanceof \Smarty\Smarty)) {
            $output .= "#data:\${$var} =";
            $output .= $ptr->hasConfigVariable($var) ? preg_replace('/\s/', '', var_export($ptr->getConfigVariable($var), true)) : 'null';
            $ptr = $ptr->parent;
        } else {
            $ptr = null;
        }
    }
    if (in_array('global', $types)) {
        $output .= "#global:\${$var} =";
        $output .= $template->getSmarty()->hasConfigVariable($var) ?
            preg_replace('/\s/', '', var_export($template->getSmarty()->getConfigVariable($var), true)) : 'null';
    }
    return $output;
}

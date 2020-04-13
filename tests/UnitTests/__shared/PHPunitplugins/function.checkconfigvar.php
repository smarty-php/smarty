<?php
/**
 * Smarty plugin for testing scopes in config vars
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

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
    $types = array('template', 'data', 'smarty');
    if (isset($params['types'])) {
        $types = (array)$params['types'];
    }
    $var = $params['var'];
    $ptr = $template;
    while ($ptr) {
        if (in_array('template', $types) && $ptr instanceof Smarty_Internal_Template) {
            $output .= "#{$ptr->source->name}:\${$var} =";
            $output .= isset($ptr->config_vars[$var]) ? preg_replace('/\s/', '', var_export($ptr->config_vars[$var], true)) : 'null';
            $ptr = $ptr->parent;
        } elseif (in_array('data', $types) && $ptr instanceof Smarty_Data) {
            $output .= "#data:\${$var} =";
            $output .= isset($ptr->config_vars[$var]) ? preg_replace('/\s/', '', var_export($ptr->config_vars[$var], true)) : 'null';
            $ptr = $ptr->parent;
        } else {
            $ptr = null;
        }
    }
    if (in_array('smarty', $types)) {
        $output .= "#Smarty:\${$var} =";
        $output .= isset($template->smarty->config_vars[ $var ]) ?
            preg_replace('/\s/', '', var_export($template->smarty->config_vars[ $var ], true)) : 'null';
    }
    return $output;
}

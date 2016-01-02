<?php
/**
 * Smarty plugin for testing scopes
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

/**
 * Smarty {checkvar}
 *
 * @param array $params parameter array
 * @param object $template template object
 *
 * @return string
 */
function smarty_function_checkvar($params, $template)
{
    $output = '';
    $var = $params['var'];
    $ptr = $template;
    while ($ptr) {
        if ($ptr instanceof Smarty_Internal_Template) {
            $output .= "template {$ptr->source->name}:var =";
            $output .= isset($ptr->tpl_vars[$var]) ? $ptr->tpl_vars[$var] : 'null';
            $output .= "\n";
            $ptr = $ptr->parent;
        } elseif ($ptr instanceof Smarty_Data) {
            $output .= "data:var =";
            $output .= isset($ptr->tpl_vars[$var]) ? $ptr->tpl_vars[$var] : 'null';
            $output .= "\n";
            $ptr = $ptr->parent;
        } else {
            $ptr = null;
        }
    }
    $output .= "Smarty:var =";
    $output .= isset($template->smarty->tpl_vars[$var]) ? $template->smarty->tpl_vars[$var] : 'null';
    $output .= "\n";
    $output .= "global:var =";
    $output .= isset(Smarty::$global_tpl_vars[$var]) ? Smarty::$global_tpl_vars[$var] : 'null';
    $output .= "\n";
    return $output;
}

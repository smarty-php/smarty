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
    $types = array('template', 'data', 'smarty', 'global');
    if (isset($params['types'])) {
        $types = (array)$params['types'];
    }
    $var = $params['var'];
    $ptr = $template;
    while ($ptr) {
        if (in_array('template', $types) && $ptr instanceof Smarty_Internal_Template) {
            $output .= "#{$ptr->source->name}:\${$var} =";
            $output .= isset($ptr->tpl_vars[$var]) ? preg_replace('/\s/', '', var_export($ptr->tpl_vars[$var]->value, true)) : '>unassigned<';
            $i = 0;
            while (isset($ptr->_cache[ 'varStack' ][ $i ])) {
                $output .= "#{$ptr->_cache[ 'varStack' ][ $i ]['name']} = ";
                $output .= isset($ptr->_cache[ 'varStack' ][ $i ][ 'tpl' ][$var]) ? preg_replace('/\s/', '', var_export($ptr->_cache[ 'varStack' ][ $i ][ 'tpl' ][$var]->value, true)) : '>unassigned<';
                $i ++;
            }
            $ptr = $ptr->parent;
        } elseif (in_array('data', $types) && $ptr instanceof Smarty_Data) {
            $output .= "#data:\${$var} =";
            $output .= isset($ptr->tpl_vars[$var]) ? preg_replace('/\s/', '', var_export($ptr->tpl_vars[$var]->value, true)) : '>unassigned<';
            $ptr = $ptr->parent;
        } else {
            $ptr = null;
        }
    }
    if (in_array('smarty', $types)) {
        $output .= "#Smarty:\${$var} =";
        $output .= isset($template->smarty->tpl_vars[ $var ]) ?
            preg_replace('/\s/', '', var_export($template->smarty->tpl_vars[ $var ]->value, true)) : '>unassigned<';
    }
    if (in_array('global', $types)) {
        $output .= "#global:\${$var} =";
        $output .= isset(Smarty::$global_tpl_vars[ $var ]) ?
            preg_replace('/\s/', '', var_export(Smarty::$global_tpl_vars[ $var ]->value, true)) : '>unassigned<';
    }
    return $output;
}

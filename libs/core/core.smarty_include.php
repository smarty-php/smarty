<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * called for included templates
 *
 * @param string $_smarty_include_tpl_file
 * @param string $_smarty_include_vars
 */
 
// $_smarty_include_tpl_file, $_smarty_include_vars

function smarty_core_smarty_include($params, &$smarty)
{
    if ($smarty->debugging) {
        $_params = array();
        require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.get_microtime.php');
        $debug_start_time = smarty_core_get_microtime($_params, $smarty);
        $smarty->_smarty_debug_info[] = array('type'      => 'template',
                                            'filename'  => $params['smarty_include_tpl_file'],
                                            'depth'     => ++$smarty->_inclusion_depth);
        $included_tpls_idx = count($smarty->_smarty_debug_info) - 1;
    }

    $smarty->_tpl_vars = array_merge($smarty->_tpl_vars, $params['smarty_include_vars']);

    // config vars are treated as local, so push a copy of the
    // current ones onto the front of the stack
    array_unshift($smarty->_config, $smarty->_config[0]);

    $_smarty_compile_path = $smarty->_get_compile_path($params['smarty_include_tpl_file']);


    if ($smarty->_is_compiled($params['smarty_include_tpl_file'], $_smarty_compile_path)
        || $smarty->_compile_resource($params['smarty_include_tpl_file'], $_smarty_compile_path))
    {
        $smarty->smarty_include($_smarty_compile_path);
    }

    // pop the local vars off the front of the stack
    array_shift($smarty->_config);

    $smarty->_inclusion_depth--;

    if ($smarty->debugging) {
        // capture time for debugging info
        $_params = array();
        require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.get_microtime.php');
        $smarty->_smarty_debug_info[$included_tpls_idx]['exec_time'] = smarty_core_get_microtime($_params, $smarty) - $debug_start_time;
    }

    if ($smarty->caching) {
        $smarty->_cache_info['template'][$params['smarty_include_tpl_file']] = true;
    }
}

/* vim: set expandtab: */

?>

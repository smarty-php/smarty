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

function smarty_core_smarty_include($params, &$this)
{
    if ($this->debugging) {
		$_params = array();
		require_once(SMARTY_DIR . 'core/core.get_microtime.php');
        $debug_start_time = smarty_core_get_microtime($_params, $this);
        $this->_smarty_debug_info[] = array('type'      => 'template',
                                            'filename'  => $params['smarty_include_tpl_file'],
                                            'depth'     => ++$this->_inclusion_depth);
        $included_tpls_idx = count($this->_smarty_debug_info) - 1;
    }

    $this->_tpl_vars = array_merge($this->_tpl_vars, $params['smarty_include_vars']);

    // config vars are treated as local, so push a copy of the
    // current ones onto the front of the stack
    array_unshift($this->_config, $this->_config[0]);

    $_smarty_compile_path = $this->_get_compile_path($params['smarty_include_tpl_file']);


    if ($this->_is_compiled($params['smarty_include_tpl_file'], $_smarty_compile_path)
        || $this->_compile_file($params['smarty_include_tpl_file'], $_smarty_compile_path))
    {
        include($_smarty_compile_path);
    }

    // pop the local vars off the front of the stack
    array_shift($this->_config);

    $this->_inclusion_depth--;

    if ($this->debugging) {
        // capture time for debugging info
		$_params = array();
		require_once(SMARTY_DIR . 'core/core.get_microtime.php');
        $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = smarty_core_get_microtime($_params, $this) - $debug_start_time;
    }

    if ($this->caching) {
        $this->_cache_info['template'][$params['smarty_include_tpl_file']] = true;
    }
}

/* vim: set expandtab: */

?>

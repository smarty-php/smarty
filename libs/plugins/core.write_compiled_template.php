<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * write the compiled template
 *
 * @param string $compile_path
 * @param string $template_compiled
 * @param integer $template_timestamp
 * @return true
 */    
function smarty_core_write_compiled_template($params, &$this)
{
	$_params = array('filename' => $params['compile_path'], 'contents' => $params['template_compiled'], 'create_dirs' => true);
	$this->_execute_core_function('write_file', $_params);		
    touch($params['compile_path'], $params['template_timestamp']);
    return true;
}

/* vim: set expandtab: */

?>

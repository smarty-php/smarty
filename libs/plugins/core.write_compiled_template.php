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
	if(!@is_writable($this->compile_dir)) {
		// compile_dir not writable, see if it exists
		if(!@is_dir($this->compile_dir)) {
			$this->trigger_error('the $compile_dir \'' . $this->compile_dir . '\' does not exist, or is not a directory.', E_USER_ERROR);
			return false;			
		}
		$this->trigger_error('unable to write to $compile_dir \'' . realpath($this->compile_dir) . '\'. Be sure $compile_dir is writable by the web server user.', E_USER_ERROR);
		return false;
	}
	
	$_params = array('filename' => $params['compile_path'], 'contents' => $params['template_compiled'], 'create_dirs' => true);
	$this->_execute_core_function('write_file', $_params);	
    touch($params['compile_path'], $params['template_timestamp']);
    return true;
}

/* vim: set expandtab: */

?>

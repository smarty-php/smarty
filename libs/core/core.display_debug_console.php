<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/*
 * Smarty debug_console function plugin
 *
 * Type:     core<br>
 * Name:     display_debug_console<br>
 * Purpose:  display the javascript debug console window
 * @param array Format: null
 * @param Smarty
 */
function smarty_core_display_debug_console($params, &$this)
{
	// we must force compile the debug template in case the environment
	// changed between separate applications.

	if(empty($this->debug_tpl)) {
		// set path to debug template from SMARTY_DIR
		$this->debug_tpl = SMARTY_DIR . 'debug.tpl';
		if($this->security && is_file($this->debug_tpl)) {
    		$this->secure_dir[] = $this->debug_tpl;
		}
	}

	$_ldelim_orig = $this->left_delimiter;
	$_rdelim_orig = $this->right_delimiter;    

	$this->left_delimiter = '{';
	$this->right_delimiter = '}';

	$_force_compile_orig = $this->force_compile;
	$this->force_compile = true;
	$_compile_id_orig = $this->_compile_id;
	$this->_compile_id = null;

	$_compile_path = $this->_get_compile_path($this->debug_tpl);
	if ($this->_process_template($this->debug_tpl, $_compile_path))
	{
		ob_start();
		include($_compile_path);
		$_results = ob_get_contents();
		ob_end_clean();
	} else {
		$_results = '';
	}
	
	$this->force_compile = $_force_compile_orig;
	$this->_compile_id = $_compile_id_orig;

	$this->left_delimiter = $_ldelim_orig;
	$this->right_delimiter = $_rdelim_orig;

	return $_results;
}

/* vim: set expandtab: */

?>

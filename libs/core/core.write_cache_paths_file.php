<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * write out the cached paths file
 *
 * @param integer $exp_time
 * @return boolean
 */
 
function smarty_core_write_cache_paths_file($params, &$this)
{
	// see if there is an pruning to do
	foreach($this->_cache_paths_max as $_max_key => $_max_val) {
		if(isset($this->_cache_paths[$_max_key])
			&& count($this->_cache_paths[$_max_key]) > $_max_val) {
			// remove the oldest (first) value
			array_unshift($this->_cache_paths[$_max_key]);
		}
	}

	$_compiled_content = function_exists('var_export') ? var_export($this->_cache_paths, true) : "unserialize('" . serialize($this->_cache_paths) . "')";
	$_compiled_content = '<?php $this->_cache_paths = ' . $_compiled_content . '; ?>';
	$_params = array('compile_path' => $this->_cache_paths_file, 'compiled_content' => $_compiled_content, 'resource_timestamp' => time());
	require_once(SMARTY_DIR . 'core/core.write_compiled_resource.php');
	smarty_core_write_compiled_resource($_params, $this);
return $_return;
}

/* vim: set expandtab: */

?>

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
 
function smarty_core_write_cache_paths_file($params, &$smarty)
{
	// see if there is an pruning to do
	foreach($smarty->_cache_paths_max as $_max_key => $_max_val) {
		if(isset($smarty->_cache_paths[$_max_key])
			&& count($smarty->_cache_paths[$_max_key]) > $_max_val) {
			// remove the oldest (first) value
			array_unshift($smarty->_cache_paths[$_max_key]);
		}
	}

	$_compiled_content = function_exists('var_export') ? var_export($smarty->_cache_paths, true) : "unserialize('" . serialize($smarty->_cache_paths) . "')";
	$_compiled_content = '<?php $smarty->_cache_paths = ' . $_compiled_content . '; ?>';
	$_params = array('compile_path' => $smarty->_cache_paths_file, 'compiled_content' => $_compiled_content, 'resource_timestamp' => time());
	require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.write_compiled_resource.php');
	smarty_core_write_compiled_resource($_params, $smarty);
return $_return;
}

/* vim: set expandtab: */

?>

<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Prepend the cache information to the cache file
 * and write it
 *
 * @param string $tpl_file
 * @param string $cache_id
 * @param string $compile_id
 * @param string $results
 * @return true|null
 */
 
 // $tpl_file, $cache_id, $compile_id, $results
 
function smarty_core_write_cache_file($params, &$this)
{

	if(!@is_writable($this->cache_dir)) {
		// cache_dir not writable, see if it exists
		if(!@is_dir($this->cache_dir)) {
			$this->trigger_error('the $cache_dir \'' . $this->cache_dir . '\' does not exist, or is not a directory.', E_USER_ERROR);
			return false;			
		}
		$this->trigger_error('unable to write to $cache_dir \'' . realpath($this->cache_dir) . '\'. Be sure $cache_dir is writable by the web server user.', E_USER_ERROR);
		return false;
	}	
	
    // put timestamp in cache header
    $this->_cache_info['timestamp'] = time();
    if ($this->cache_lifetime > -1){
        // expiration set
        $this->_cache_info['expires'] = $this->_cache_info['timestamp'] + $this->cache_lifetime;
    } else {
        // cache will never expire
        $this->_cache_info['expires'] = -1;
    }

    // collapse {nocache...}-tags
    $params['results'] = preg_replace('!((\{nocache\:([0-9a-f]{32})#(\d+)\})'
                                      .'.*'
                                      .'{/nocache\:\\3#\\4\})!Us'
                                      ,'\\2'
                                      ,$params['results']);
    $this->_cache_info['cache_serials'] = $this->_cache_serials;

    // prepend the cache header info into cache file
    $params['results'] = serialize($this->_cache_info)."\n".$params['results'];

    if (!empty($this->cache_handler_func)) {
        // use cache_handler function
        call_user_func_array($this->cache_handler_func,
                       array('write', &$this, &$params['results'], $params['tpl_file'], $params['cache_id'], $params['compile_id']));
    } else {
        // use local cache file
        $_auto_id = $this->_get_auto_id($params['cache_id'], $params['compile_id']);
        $_cache_file = $this->_get_auto_filename($this->cache_dir, $params['tpl_file'], $_auto_id);
		$_params = array('filename' => $_cache_file, 'contents' => $params['results'], 'create_dirs' => true);
		require_once(SMARTY_DIR . 'core/core.write_file.php');
        smarty_core_write_file($_params, $this);
        return true;
    }
}

/* vim: set expandtab: */

?>

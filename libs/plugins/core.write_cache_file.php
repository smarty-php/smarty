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
    // put timestamp in cache header
    $this->_cache_info['timestamp'] = time();
    if ($this->cache_lifetime > -1){
        // expiration set
        $this->_cache_info['expires'] = $this->_cache_info['timestamp'] + $this->cache_lifetime;
    } else {
        // cache will never expire
        $this->_cache_info['expires'] = -1;
    }

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
		$this->_execute_core_function('write_file', $_params);
        return true;
    }
}

/* vim: set expandtab: */

?>

<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * read a cache file, determine if it needs to be
 * regenerated or not
 *
 * @param string $tpl_file
 * @param string $cache_id
 * @param string $compile_id
 * @param string $results
 * @return boolean
 */    
 
//  $tpl_file, $cache_id, $compile_id, &$results
 
function smarty_core_read_cache_file(&$params, &$smarty)
{
    static  $content_cache = array();

    if ($smarty->force_compile) {
        // force compile enabled, always regenerate
        return false;
    }

    if (isset($content_cache[$params['tpl_file'].','.$params['cache_id'].','.$params['compile_id']])) {
        list($params['results'], $smarty->_cache_info) = $content_cache[$params['tpl_file'].','.$params['cache_id'].','.$params['compile_id']];
        return true;
    }

    if (!empty($smarty->cache_handler_func)) {
        // use cache_handler function
        call_user_func_array($smarty->cache_handler_func,
                             array('read', &$smarty, &$params['results'], $params['tpl_file'], $params['cache_id'], $params['compile_id']));
    } else {
        // use local cache file
        $_auto_id = $smarty->_get_auto_id($params['cache_id'], $params['compile_id']);
        $_cache_file = $smarty->_get_auto_filename($smarty->cache_dir, $params['tpl_file'], $_auto_id);
        $params['results'] = $smarty->_read_file($_cache_file);
    }

    if (empty($params['results'])) {
        // nothing to parse (error?), regenerate cache
        return false;
    }

    $cache_split = explode("\n", $params['results'], 2);
    $cache_header = $cache_split[0];

    $smarty->_cache_info = unserialize($cache_header);

    if ($smarty->caching == 2 && isset ($smarty->_cache_info['expires'])){
        // caching by expiration time
        if ($smarty->_cache_info['expires'] > -1 && (time() > $smarty->_cache_info['expires'])) {
        // cache expired, regenerate
        return false;
        }
    } else {
        // caching by lifetime
        if ($smarty->cache_lifetime > -1 && (time() - $smarty->_cache_info['timestamp'] > $smarty->cache_lifetime)) {
        // cache expired, regenerate
        return false;
        }
    }

    if ($smarty->compile_check) {
		require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.fetch_resource_info.php');
        foreach (array_keys($smarty->_cache_info['template']) as $_template_dep) {
			$_params = array('resource_name' => $_template_dep);
			smarty_core_fetch_resource_info($_params, $smarty);
            if ($smarty->_cache_info['timestamp'] < $_params['resource_timestamp']) {
                // template file has changed, regenerate cache
                return false;
            }
        }

        if (isset($smarty->_cache_info['config'])) {
			require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.fetch_resource_info.php');
            foreach (array_keys($smarty->_cache_info['config']) as $_config_dep) {
				$_params = array('resource_name' => $_config_dep);
				smarty_core_fetch_resource_info($_params, $smarty);
            	if ($smarty->_cache_info['timestamp'] < $_params['resource_timestamp']) {
                	// config file has changed, regenerate cache
                	return false;
            	}
            }
        }
    }

    foreach ($smarty->_cache_info['cache_serials'] as $_include_file_path=>$_cache_serial) {
        if (empty($smarty->_cache_serials[$_include_file_path])) {
            $smarty->smarty_include($_include_file_path, true);
        }
        
        if ($smarty->_cache_serials[$_include_file_path] != $_cache_serial) {
            /* regenerate */
            return false;
        }
    }
    $params['results'] = $cache_split[1];
    $content_cache[$params['tpl_file'].','.$params['cache_id'].','.$params['compile_id']] = array($params['results'], $smarty->_cache_info);

    return true;
}

/* vim: set expandtab: */

?>

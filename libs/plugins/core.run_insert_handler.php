<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Handle insert tags
 *
 * @param array $args
 * @return string
 */    
function smarty_core_run_insert_handler($params, &$this)
{
	
    if ($this->debugging) {
		$_params = array();
        $_debug_start_time = $this->_execute_core_function('get_microtime', $_params);
    }

    if ($this->caching) {
        $_arg_string = serialize($params['args']);
        $_name = $params['args']['name'];
        if (!isset($this->_cache_info['insert_tags'][$_name])) {
            $this->_cache_info['insert_tags'][$_name] = array('insert',
                                                             $_name,
                                                             $this->_plugins['insert'][$_name][1],
                                                             $this->_plugins['insert'][$_name][2],
                                                             !empty($params['args']['script']) ? true : false);
        }
        return $this->_smarty_md5."{insert_cache $_arg_string}".$this->_smarty_md5;
    } else {
        if (isset($params['args']['script'])) {				
			$_params = array('file_path' => $this->_dequote($params['args']['script']));
			if(!$this->_execute_core_function('get_php_resource', $_params)) {
				return false;
			}

            if ($_params['resource_type'] == 'file') {
                include_once($_params['php_resource']);
            } else {
                eval($_params['php_resource']);
            }
            unset($params['args']['script']);
        }

        $_funcname = $this->_plugins['insert'][$params['args']['name']][0];
        $_content = $_funcname($params['args'], $this);
        if ($this->debugging) {
			$_params = array();
            $this->_smarty_debug_info[] = array('type'      => 'insert',
                                                'filename'  => 'insert_'.$params['args']['name'],
                                                'depth'     => $this->_inclusion_depth,
                                                'exec_time' => $this->_execute_core_function('get_microtime', $_params) - $_debug_start_time);
        }

        if (!empty($params['args']["assign"])) {
            $this->assign($params['args']["assign"], $_content);
        } else {
            return $_content;
        }
    }
}

/* vim: set expandtab: */

?>

<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Replace cached inserts with the actual results
 *
 * @param string $results
 * @return string
 */    
function smarty_core_process_cached_inserts($params, &$this)
{
    preg_match_all('!'.$this->_smarty_md5.'{insert_cache (.*)}'.$this->_smarty_md5.'!Uis',
                   $params['results'], $match);
    list($cached_inserts, $insert_args) = $match;

    for ($i = 0, $for_max = count($cached_inserts); $i < $for_max; $i++) {
        if ($this->debugging) {
			$_params = array();
            $debug_start_time = $this->_execute_core_function('get_microtime', $_params);
        }

        $args = unserialize($insert_args[$i]);
        $name = $args['name'];

        if (isset($args['script'])) {
			$_params = array('file_path' => $this->_dequote($args['script']));
			if(!$this->_execute_core_function('get_php_resource', $_params)) {
				return false;
			}
			$resource_type = $_params['resource_type'];
			$php_resource = $_params['php_resource'];


            if ($resource_type == 'file') {
                include_once($php_resource);
            } else {
                eval($php_resource);
            }
        }

        $function_name = $this->_plugins['insert'][$name][0];
        $replace = $function_name($args, $this);

        $params['results'] = str_replace($cached_inserts[$i], $replace, $params['results']);
        if ($this->debugging) {
			$_params = array();
            $this->_smarty_debug_info[] = array('type'      => 'insert',
                                                'filename'  => 'insert_'.$name,
                                                'depth'     => $this->_inclusion_depth,
                                                'exec_time' => $this->_execute_core_function('get_microtime', $_params) - $debug_start_time);
        }
    }

    return $params['results'];
}

/* vim: set expandtab: */

?>

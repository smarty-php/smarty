<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Retrieves PHP script resource
 *
 * sets $php_resource to the returned resource
 * @param string $resource
 * @param string $resource_type
 * @param  $php_resource
 * @return boolean
 */    
 
function smarty_core_get_php_resource(&$params, &$this)
{
	
	$params['file_base_path'] = $this->trusted_dir;	
	$this->_execute_core_function('parse_file_path', $params);
		
    /*
     * Find out if the resource exists.
     */

    if ($params['resource_type'] == 'file') {
        $_readable = false;
        if(file_exists($params['resource_name']) && is_readable($params['resource_name'])) {
            $_readable = true;
        } else {			
            // test for file in include_path
			$_params = array('file_path' => $params['resource_name']);
            if($this->_execute_core_function('get_include_path', $_params)) {				
				$_include_path = $_params['new_file_path'];
				$_readable = true;
            }
        }
    } else if ($params['resource_type'] != 'file') {
		$_template_source = null;
        $_readable = $this->_plugin_implementation_exists($this->_plugins['resource'][$params['resource_type']][0][0])
            && call_user_func_array($this->_plugins['resource'][$params['resource_type']][0][0],
                                    array($params['resource_name'], &$_template_source, &$this));
    }

    /*
     * Set the error function, depending on which class calls us.
     */
    if (method_exists($this, '_syntax_error')) {
        $_error_funcc = '_syntax_error';
    } else {
        $_error_funcc = 'trigger_error';
    }

    if ($_readable) {
        if ($this->security) {
            if (!$this->_execute_core_function('is_trusted',$params)) {
                $this->$_error_funcc('(secure mode) ' . $params['resource_type'] . ':' . $params['resource_name'] . ' is not trusted');
                return false;
            }
        }
    } else {
        $this->$_error_funcc($params['resource_type'] . ':' . $params['resource_name'] . ' is not readable');
        return false;
    }

    if ($params['resource_type'] == 'file') {
        $params['php_resource'] = $params['resource_name'];
    } else {
        $params['php_resource'] = $_template_source;
    }
    return true;
}

/* vim: set expandtab: */

?>

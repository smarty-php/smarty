<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * parse out the type and name from the template resource
 *
 * @param string $file_base_path
 * @param string $file_path
 * @param string $resource_type
 * @param string $resource_name
 * @return boolean
 */
 
//  $file_base_path, $file_path, &$resource_type, &$resource_name

function smarty_core_parse_file_path(&$params, &$this)
{
		
    // split tpl_path by the first colon
    $_file_path_parts = explode(':', $params['file_path'], 2);
		
    if (count($_file_path_parts) == 1) {
        // no resource type given
        $params['resource_type'] = $this->default_resource_type;
        $params['resource_name'] = $_file_path_parts[0];
    } else {
		if(strlen($_file_path_parts[0]) == 1) {
			// 1 char is not resource type, but part of filepath
        	$params['resource_type'] = $this->default_resource_type;
        	$params['resource_name'] = $params['file_path'];	
		} else {
        	$params['resource_type'] = $_file_path_parts[0];
        	$params['resource_name'] = $_file_path_parts[1];
		}
    }	
	
    if ($params['resource_type'] == 'file') {
        if (!preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $params['resource_name'])) {
            // relative pathname to $params['file_base_path']
            // use the first directory where the file is found
			$_file_base_path = $params['file_base_path'];
			settype($_file_base_path, 'array');
            foreach ($_file_base_path as $_curr_path) {
                $_fullpath = $_curr_path . DIRECTORY_SEPARATOR . $params['resource_name'];
                if (file_exists($_fullpath) && is_file($_fullpath)) {
                    $params['resource_name'] = $_fullpath;
                    return true;
                }
                // didn't find the file, try include_path
				$_params = array('file_path' => $_fullpath);
            	if($this->_execute_core_function('get_include_path', $_params)) {				
					$params['resource_name'] = $_params['new_file_path'];
					return true;
                }
            }
            return false;
        }
	} else {
		$_params = array('type' => $params['resource_type']);
		$this->_execute_core_function('load_resource_plugin', $_params);
    }

    return true;
}

/* vim: set expandtab: */

?>

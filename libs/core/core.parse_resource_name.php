<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * parse out the type and name from the resource
 *
 * @param string $resource_base_path
 * @param string $resource_name
 * @param string $resource_type
 * @param string $resource_name
 * @return boolean
 */
 
//  $resource_base_path, $resource_name, &$resource_type, &$resource_name

function smarty_core_parse_resource_name(&$params, &$smarty)
{
		
    // split tpl_path by the first colon
    $_resource_name_parts = explode(':', $params['resource_name'], 2);
		
    if (count($_resource_name_parts) == 1) {
        // no resource type given
        $params['resource_type'] = $smarty->default_resource_type;
        $params['resource_name'] = $_resource_name_parts[0];
    } else {
		if(strlen($_resource_name_parts[0]) == 1) {
			// 1 char is not resource type, but part of filepath
        	$params['resource_type'] = $smarty->default_resource_type;
        	$params['resource_name'] = $params['resource_name'];	
		} else {
        	$params['resource_type'] = $_resource_name_parts[0];
        	$params['resource_name'] = $_resource_name_parts[1];
		}
    }	
		
    if ($params['resource_type'] == 'file') {
        if (!preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $params['resource_name'])) {
            // relative pathname to $params['resource_base_path']
            // use the first directory where the file is found
			$_resource_base_path = isset($params['resource_base_path']) ? $params['resource_base_path'] : array($smarty->template_dir, '.');
			settype($_resource_base_path, 'array');
            foreach ($_resource_base_path as $_curr_path) {
                $_fullpath = $_curr_path . DIRECTORY_SEPARATOR . $params['resource_name'];
                if (file_exists($_fullpath) && is_file($_fullpath)) {
                    $params['resource_name'] = $_fullpath;
                    return true;
                }
                // didn't find the file, try include_path
				$_params = array('file_path' => $_fullpath);
				require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.get_include_path.php');
            	if(smarty_core_get_include_path($_params, $smarty)) {				
					$params['resource_name'] = $_params['new_file_path'];
					return true;
                }
            }
            return false;
        }
	} else {
		$_params = array('type' => $params['resource_type']);
		require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.load_resource_plugin.php');
		smarty_core_load_resource_plugin($_params, $smarty);
    }

    return true;
}

/* vim: set expandtab: */

?>

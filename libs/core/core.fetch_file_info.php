<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * fetch the template info. Gets timestamp, and source
 * if get_source is true
 *
 * sets $file_source to the source of the template, and
 * $file_timestamp to its time stamp
 * @param string $file_path
 * @param string $file_source
 * @param integer $file_timestamp
 * @param boolean $get_source
 * @param boolean $quiet
 * @return boolean
 */

// $tpl_path, &$template_source, &$template_timestamp, $get_source = true, $quiet = false

function smarty_core_fetch_file_info(&$params, &$this)
{
		
	if(!isset($params['get_source'])) { $params['get_source'] = true; }
	if(!isset($params['quiet'])) { $params['quiet'] = false; }
	
    $_return = false;
	$_params = array('file_path' => $params['file_path']) ;
	require_once(SMARTY_DIR . 'core/core.parse_file_path.php');
    if (smarty_core_parse_file_path($_params, $this)) {
		$_resource_type = $_params['resource_type'];
		$_resource_name = $_params['resource_name'];
        switch ($_resource_type) {
            case 'file':
                if ($params['get_source']) {
                    $params['file_source'] = $this->_read_file($_resource_name);
                }
                $params['file_timestamp'] = filemtime($_resource_name);
                $_return = true;
                break;

            default:                    
                // call resource functions to fetch the template source and timestamp
                if ($params['get_source']) {
                    $_source_return = isset($this->_plugins['resource'][$_resource_type]) &&
                        call_user_func_array($this->_plugins['resource'][$_resource_type][0][0],
                                             array($_resource_name, &$params['file_source'], &$this));
                } else {
                    $_source_return = true;
                }

                $_timestamp_return = isset($this->_plugins['resource'][$_resource_type]) &&
                    call_user_func_array($this->_plugins['resource'][$_resource_type][0][1],
                                         array($_resource_name, &$params['file_timestamp'], &$this));

                $_return = $_source_return && $_timestamp_return;
                break;
        }
    }

    if (!$_return) {
        // see if we can get a template with the default template handler
        if (!empty($this->default_template_handler_func)) {
            if (!$this->_plugin_implementation_exists($this->default_template_handler_func)) {
                $this->trigger_error("default template handler function \"$this->default_template_handler_func\" doesn't exist.");
            } else {
                $_return = call_user_func_array(
                    $this->default_template_handler_func,
                    array($_resource_type, $_resource_name, &$params['file_source'], &$params['file_timestamp'], &$this));
			}
        }
    }

	require_once(SMARTY_DIR . 'core/core.is_secure.php');
    if (!$_return) {
        if (!$params['quiet']) {
            $this->trigger_error('unable to read template resource: "' . $params['file_path'] . '"');
        }
    } else if ($_return && $this->security && !smarty_core_is_secure($_params, $this)) {
        if (!$params['quiet'])
            $this->trigger_error('(secure mode) accessing "' . $params['file_path'] . '" is not allowed');
        $params['file_source'] = null;
        $params['file_timestamp'] = null;
        return false;
    }
    return $_return;
}


/* vim: set expandtab: */

?>

<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Smarty {config_load} function plugin
 *
 * Type:     compiler<br>
 * Name:     config_load<br>
 * Purpose:  load config file vars
 * @link http://smarty.php.net/manual/en/language.function.config.load.php {config_load}
 *       (Smarty online manual)
 * @param array Format:
 * <pre>
 * array('file' => required config file name,
 *       'section' => optional config file section to load
 *       'scope' => local/parent/global
 *       'global' => overrides scope, setting to parent if true)
 * </pre>
 * @param Smarty
 */
function smarty_compiler_config_load($params, &$smarty)
{
		$params = $smarty->_parse_attrs($params, false);
	
        if ($smarty->debugging) {
			$_params = array();
            $_debug_start_time = $smarty->_execute_core_function('get_microtime', $_params);
        }

		$_file = isset($params['file']) ? $smarty->_dequote($params['file']) : null;
		$_section = isset($params['section']) ? $smarty->_dequote($params['section']) : null;
		$_scope = isset($params['scope']) ? $smarty->_dequote($params['scope']) : 'global';
		$_global = isset($params['global']) ? $smarty->_dequote($params['global']) : false;

        if (!isset($_file) || strlen($_file) == 0) {
            $smarty->_syntax_error("missing 'file' attribute in config_load tag", E_USER_ERROR, __FILE__, __LINE__);
        }

        if (isset($_scope)) {
            if ($_scope != 'local' &&
                $_scope != 'parent' &&
                $_scope != 'global') {
                $smarty->_syntax_error("invalid 'scope' attribute value", E_USER_ERROR, __FILE__, __LINE__);
            }
        } else {
            if ($_global) {
                $_scope = 'parent';
			} else {
                $_scope = 'local';
			}
        }		
			
        if(@is_dir($smarty->config_dir)) {
            $_config_dir = $smarty->config_dir;            
        } else {
            // config_dir not found, try include_path
			$_params = array('file_path' => $smarty->config_dir);
            $smarty->_execute_core_function('get_include_path', $_params);
			$_config_dir = $_params['new_file_path'];
        }

        if(!is_object($smarty->_conf_obj)) {
            require_once SMARTY_DIR . $smarty->config_class . '.class.php';
            $smarty->_conf_obj = new $smarty->config_class($_config_dir);
            $smarty->_conf_obj->overwrite = $smarty->config_overwrite;
            $smarty->_conf_obj->booleanize = $smarty->config_booleanize;
            $smarty->_conf_obj->read_hidden = $smarty->config_read_hidden;
            $smarty->_conf_obj->fix_newlines = $smarty->config_fix_newlines;
            $smarty->_conf_obj->set_path = $_config_dir;
        }
        $_config_vars = array_merge($smarty->_conf_obj->get($_file),
                $smarty->_conf_obj->get($_file, $_section));

        if ($smarty->caching) {
            $smarty->_cache_info['config'][$_file] = true;
        }

        $smarty->_config[0]['vars'] = @array_merge($smarty->_config[0]['vars'], $_config_vars);
        $smarty->_config[0]['files'][$_file] = true;
        
        if ($_scope == 'parent') {
                $smarty->_config[1]['vars'] = @array_merge($smarty->_config[1]['vars'], $_config_vars);
                $smarty->_config[1]['files'][$_file] = true;
        } else if ($_scope == 'global') {
            for ($i = 1, $for_max = count($smarty->_config); $i < $for_max; $i++) {
                    $smarty->_config[$i]['vars'] = @array_merge($smarty->_config[$i]['vars'], $_config_vars);
                    $smarty->_config[$i]['files'][$_file] = true;
            }
        }
		
        if ($smarty->debugging) {
			$_params = array();
            $smarty->_smarty_debug_info[] = array('type'      => 'config',
                                                'filename'  => $_file.' ['.$_section.'] '.$_scope,
                                                'depth'     => $smarty->_inclusion_depth,
                                                'exec_time' => $smarty->_execute_core_function('get_microtime', $_params) - $_debug_start_time);
        }
	
}

/* vim: set expandtab: */

?>

<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Smarty {config_load} function plugin
 *
 * Type:     function<br>
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
function smarty_function_config_load($params, &$smarty)
{
        if ($smarty->debugging) {
            $_debug_start_time = $smarty->_get_microtime();
        }

		$_file = isset($params['file']) ? $params['file'] : null;
		$_section = isset($params['section']) ? $params['section'] : null;
		$_scope = isset($params['scope']) ? $params['scope'] : 'global';
		$_global = isset($params['global']) ? $params['global'] : false;

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
            $smarty->_get_include_path($smarty->config_dir, $_config_dir);
        }

        $_file_path = str_replace('//', '/' ,$_config_dir . '/' . $_file);
        
        // get path to compiled object file
        if(isset($_section)) {
               $_compile_file = $smarty->_get_auto_filename($smarty->compile_dir, $_section . ' ' . $_file);
        } else {
               $_compile_file = $smarty->_get_auto_filename($smarty->compile_dir, $_file);
        }

        // need to compile config file?
        if($smarty->force_compile || !file_exists($_compile_file) ||
            ($smarty->compile_check &&
                file_exists($_file_path) &&
                ( filemtime($_compile_file) != filemtime($_file_path) ))) {
            $_compile_config = true;
        } else {
            include($_compile_file);
			$_compile_config = empty($_config_vars);
        }
		
        if($_compile_config) {
            if(!is_object($smarty->_conf_obj)) {
                require_once SMARTY_DIR . $smarty->config_class . '.class.php';
                $smarty->_conf_obj = new $smarty->config_class($_config_dir);
                $smarty->_conf_obj->overwrite = $smarty->config_overwrite;
                $smarty->_conf_obj->booleanize = $smarty->config_booleanize;
                $smarty->_conf_obj->read_hidden = $smarty->config_read_hidden;
                $smarty->_conf_obj->fix_newlines = $smarty->config_fix_newlines;
                $smarty->_conf_obj->set_path = $_config_dir;
            }
            if($_config_vars = array_merge($smarty->_conf_obj->get($_file),
                    $smarty->_conf_obj->get($_file, $_section))) {
                if(function_exists('var_export')) {
                    $_compile_data = '<?php $_config_vars = ' . var_export($_config_vars, true) . '; return true; ?>';                    
                } else {
                    $_compile_data = '<?php $_config_vars = unserialize(\'' . str_replace('\'','\\\'', serialize($_config_vars)) . '\'); return true; ?>';
                }
                $smarty->_write_file($_compile_file, $_compile_data, true);
                touch($_compile_file,filemtime($_file_path));
            }
        }

        if ($smarty->caching) {
            $smarty->_cache_info['config'][] = $_file;
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
            $smarty->_smarty_debug_info[] = array('type'      => 'config',
                                                'filename'  => $_file.' ['.$_section.'] '.$_scope,
                                                'depth'     => $smarty->_inclusion_depth,
                                                'exec_time' => $smarty->_get_microtime() - $_debug_start_time);
        }
	
}

/* vim: set expandtab: */

?>

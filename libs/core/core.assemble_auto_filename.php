<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * get a concrete filename for automagically created content
 *
 * @param string $auto_base
 * @param string $auto_source
 * @param string $auto_id
 * @return string
 * @staticvar string|null
 * @staticvar string|null
 */    
function smarty_core_assemble_auto_filename($params, &$smarty)
{	

	$_compile_dir_sep =  $smarty->use_sub_dirs ? DIRECTORY_SEPARATOR : '^';
		
    if(@is_dir($params['auto_base'])) {
        $_return = $params['auto_base'] . DIRECTORY_SEPARATOR;
    } else {
        // auto_base not found, try include_path
		$_params = array('file_path' => $params['auto_base']);
		require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.get_include_path.php');
        smarty_core_get_include_path($_params, $smarty);
        $_return = isset($_params['new_file_path']) ? $_params['new_file_path'] . DIRECTORY_SEPARATOR : null;
    }

    if(isset($params['auto_id'])) {
        // make auto_id safe for directory names
        $params['auto_id'] = str_replace('%7C','|',(urlencode($params['auto_id'])));
        // split into separate directories
        $params['auto_id'] = str_replace('|', $_compile_dir_sep, $params['auto_id']);
        $_return .= $params['auto_id'] . $_compile_dir_sep;
    }

    if(isset($params['auto_source'])) {
        // make source name safe for filename
        $_filename = urlencode(basename($params['auto_source']));
        $_crc32 = crc32($params['auto_source']) . $_compile_dir_sep;
        // prepend %% to avoid name conflicts with
        // with $params['auto_id'] names
        $_crc32 = '%%' . substr($_crc32,0,3) . $_compile_dir_sep . '%%' . $_crc32;
        $_return .= $_crc32 . $_filename;
    }
	
    return $_return;
}

/* vim: set expandtab: */

?>

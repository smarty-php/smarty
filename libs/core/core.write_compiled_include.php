<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Extract non-cacheable parts out of compiled template and write it
 *
 * @param string $compile_path
 * @param string $template_compiled
 * @param integer $template_timestamp
 * @return boolean
 */
 
function smarty_core_write_compiled_include($params, &$smarty)
{
    $_tag_start = 'if \(\$smarty->caching\) \{ echo \'\{nocache\:('.$params['cache_serial'].')#(\d+)\}\';\}';
    $_tag_end   = 'if \(\$smarty->caching\) \{ echo \'\{/nocache\:(\\2)#(\\3)\}\';\}';

    preg_match_all('!('.$_tag_start.'(.*)'.$_tag_end.')!Us', 
                   $params['compiled_content'], $_match_source, PREG_SET_ORDER);

    // no nocache-parts found: done
    if (count($_match_source)==0) return;
    
    // convert the matched php-code to functions
    $_include_compiled = "<?php /* funky header here */\n\n";

    $_compile_path = $params['include_file_path'];

    $smarty->_cache_serials[$_compile_path] = $smarty->_cache_serial;
    $_include_compiled .= "\$smarty->_cache_serials['".$smarty->_cache_include."'] = '".$smarty->_cache_serial."';\n\n?>";

    $_include_compiled .= $params['plugins_code'];
    $_include_compiled .= "<?";
    for ($_i = 0, $_for_max = count($_match_source); $_i < $_for_max; $_i++) {
        $_match =& $_match_source[$_i];
        $_include_compiled .= "
function _smarty_tplfunc_$_match[2]_$_match[3](&\$smarty)
{
$_match[4]
}

";
    }
    $_include_compiled .= "\n\n?>\n";
    
	$_params = array('filename' => $_compile_path, 
                     'contents' => $_include_compiled, 'create_dirs' => true);
    
    require_once(SMARTY_DIR . 'core' . DIRECTORY_SEPARATOR . 'core.write_file.php');
    smarty_core_write_file($_params, $smarty);
	return true;
}


?>

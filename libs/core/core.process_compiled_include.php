<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * Replace nocache-tags by results of the corresponding non-cacheable
 * functions and return it
 *
 * @param string $compiled_tpl
 * @param string $cached_source
 * @return string
 */
 
function smarty_core_process_compiled_include($params, &$this)
{
    $_return = $params['results'];
    foreach ($this->_cache_serials as $_include_file_path=>$_cache_serial) {
        include_once($_include_file_path);
        $_return = preg_replace_callback('!(\{nocache\:('.$_cache_serial.')#(\d+)\})!s',
                                         array(&$this, '_process_compiled_include_callback'),
                                         $_return);
    }
    return $_return;
}

?>

<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * called for included php files within templates
 *
 * @param string $_smarty_include_php_file
 * @param string $_smarty_assign variable to assign the included template's
 *               output into
 * @param boolean $_smarty_once uses include_once if this is true
 * @param array $_smarty_include_vars associative array of vars from
 *              {include file="blah" var=$var}
 */    

// 	$file, $assign, $once, $_smarty_include_vars	 		  
		 
function smarty_core_smarty_include_php($params, &$this)
{
	$_params = array('file_path' => $params['smarty_file']);
	$this->_execute_core_function('get_php_resource', $_params);
	$_smarty_resource_type = $_params['resource_type'];
	$_smarty_php_resource = $_params['php_resource'];

    extract($params['smarty_include_vars'], EXTR_PREFIX_SAME, 'include_php_');

    if (!empty($params['smarty_assign'])) {
        ob_start();
        if ($_smarty_resource_type == 'file') {
            if($params['smarty_once']) {
                include_once($_smarty_php_resource);
            } else {
                include($_smarty_php_resource);                    
            }
        } else {
            eval($_smarty_php_resource);
        }
        $this->assign($params['smarty_assign'], ob_get_contents());
        ob_end_clean();
    } else {
        if ($_smarty_resource_type == 'file') {
            if($params['smarty_once']) {
                include_once($_smarty_php_resource);
            } else {
                include($_smarty_php_resource);                    
            }
        } else {
            eval($_smarty_php_resource);
        }
    }
}


/* vim: set expandtab: */

?>

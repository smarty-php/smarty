<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**
 * determines if a resource is secure or not.
 *
 * @param string $resource_type
 * @param string $resource_name
 * @return boolean
 */
 
//  $resource_type, $resource_name
 
function smarty_core_is_secure($params, &$smarty)
{
    if (!$smarty->security || $smarty->security_settings['INCLUDE_ANY']) {
        return true;
    }

    $_smarty_secure = false;
    if ($params['resource_type'] == 'file') {
        if (!empty($smarty->secure_dir)) {
            $_rp = realpath($params['resource_name']);
            foreach ((array)$smarty->secure_dir as $curr_dir) {
                if ( !empty($curr_dir) && is_readable ($curr_dir)) {
                    $_cd = realpath($curr_dir);
                    if (strncmp($_rp, $_cd, strlen($_cd)) == 0
                        && $_rp{strlen($_cd)} == DIRECTORY_SEPARATOR ) {
                        $_smarty_secure = true;
                        break;
                    }
                }
            }
        }
    } else {
        // resource is not on local file system
        $_smarty_secure = call_user_func_array(
            $smarty->_plugins['resource'][$params['resource_type']][0][2],
            array($params['resource_name'], &$_smarty_secure, &$smarty));
    }

    return $_smarty_secure;
}

/* vim: set expandtab: */

?>

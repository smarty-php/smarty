<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */

/**#@+
 * @access private
 */
/**
 * determines if a resource is trusted or not
 *
 * @param string $resource_type
 * @param string $resource_name
 * @return boolean
 */    
 
 // $resource_type, $resource_name

function smarty_core_is_trusted($params, &$this)
{
    $_smarty_trusted = false;
    if ($params['resource_type'] == 'file') {
        if (!empty($this->trusted_dir)) {
            // see if template file is within a trusted directory. If so,
            // disable security during the execution of the template.

            if (!empty($this->trusted_dir)) {
                foreach ((array)$this->trusted_dir as $curr_dir) {
                    if (!empty($curr_dir) && is_readable ($curr_dir)) {
                        if (substr(realpath($params['resource_name']),0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
                            $_smarty_trusted = true;
                            break;
                        }
                    }
                }
            }
        }
    } else {
        // resource is not on local file system
        $_smarty_trusted = call_user_func_array($this->_plugins['resource'][$params['resource_type']][0][3],
                                                array($params['resource_name'], $this));
    }

    return $_smarty_trusted;
}

/* vim: set expandtab: */

?>

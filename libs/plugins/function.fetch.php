<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     fetch
 * Purpose:  fetch file, web or ftp data and display results
 * -------------------------------------------------------------
 */
function smarty_function_fetch($args, &$smarty_obj)
{
    extract($args);

    if (empty($file)) {
        $smarty_obj->_trigger_error_msg("parameter 'file' cannot be empty");
        return;
    }

    if ($smarty_obj->security && !preg_match('!^(http|ftp)://!', $file)) {
        // make sure fetched file comes from secure directory
        foreach ($smarty_obj->secure_dir as $curr_dir) {
            if (substr(realpath($file), 0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
                $resource_is_secure = true;
                break;
            }
        }
        if (!$resource_is_secure) {
            $smarty_obj->_trigger_error_msg("(secure mode) fetch '$file' is not allowed");
            return;
        }
        if (!@is_readable($file)) {
            $smarty_obj->_trigger_error_msg("fetch cannot read file '$file'");
            return;
        }
    }


    if (!empty($assign)) {
        ob_start();
        readfile($file);
        $smarty_obj->assign($assign,ob_get_contents());
        ob_end_clean();
    } else {
        readfile($file);
    }
}

/* vim: set expandtab: */

?>

<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     fetch
 * Purpose:  fetch file, web or ftp data and display results
 * -------------------------------------------------------------
 */
function smarty_function_fetch($params, &$smarty)
{
    extract($params);

    if (empty($file)) {
        $smarty->trigger_error("parameter 'file' cannot be empty");
        return;
    }

    if ($smarty->security && !preg_match('!^(http|ftp)://!', $file)) {
        // make sure fetched file comes from secure directory
        foreach ($smarty->secure_dir as $curr_dir) {
            if (substr(realpath($file), 0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
                $resource_is_secure = true;
                break;
            }
        }
        if (!$resource_is_secure) {
            $smarty->trigger_error("(secure mode) fetch '$file' is not allowed");
            return;
        }
        if (!@is_readable($file)) {
            $smarty->trigger_error("fetch cannot read file '$file'");
            return;
        }
    }


    if (!empty($assign)) {
        ob_start();
        readfile($file);
        $smarty->assign($assign,ob_get_contents());
        ob_end_clean();
    } else {
        readfile($file);
    }
}

/* vim: set expandtab: */

?>

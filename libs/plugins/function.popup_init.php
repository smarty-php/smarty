<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     popup_init
 * Purpose:  initialize overlib
 * -------------------------------------------------------------
 */
function smarty_function_popup_init($args, &$smarty_obj)
{
    // be sure to place overlib.js where Smarty can locate it.
    // overlib.js came with the distribution of Smarty.
    extract($args);
    echo '<div id="overDiv" style="position:absolute; visibility:hidden; z-index:1000;"></div>'."\n";
    if (empty($src)) {
        echo '<script language="JavaScript">'."\n".'<!--'."\n";
        readfile(SMARTY_DIR."overlib.js",1);
        echo '// -->'."\n".'</script>'."\n";
    } else {
        echo '<script language="JavaScript" src="'.$src.'"></script>'."\n";
    }
    return;
}

/* vim: set expandtab: */

?>

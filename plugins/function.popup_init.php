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
    echo '<DIV ID="overDiv" STYLE="position:absolute; visibility:hidden; z-index:1000;"></DIV>'."\n".'<SCRIPT LANGUAGE=javascript>'."\n".'<!--'."\n";
    readfile(SMARTY_DIR."overlib.js",1);
    echo '// -->'."\n".'</SCRIPT>'."\n";
    return;
}

/* vim: set expandtab: */

?>

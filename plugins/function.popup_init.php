<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     popup_init
 * Purpose:  initialize overlib
 * -------------------------------------------------------------
 */
function smarty_function_popup_init($params, &$smarty)
{
    if (!empty($params['src'])) {
    	echo '<div id="overDiv" style="position:absolute; visibility:hidden; z-index:1000;"></div>'."\n";
        echo '<script language="JavaScript" src="'.$params['src'].'"></script>'."\n";
    } else {
        $smarty->trigger_error("popup_init: missing src parameter");
    }
}

/* vim: set expandtab: */

?>

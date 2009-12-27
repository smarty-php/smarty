<?php

/**
* Smarty method Unregister_Modifier
* 
* Unregister a Smarty modifier plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a Smarty modifier plugin
*/

/**
* Unregisters modifier
* 
* @param string $modifier name of template modifier
*/
function  Smarty_Method_Unregister_Modifier($smarty, $modifier)
{
    if (isset($smarty->registered_plugins['modifier'][$modifier])) {
        unset($smarty->registered_plugins['modifier'][$modifier]);
    } 
} 

?>

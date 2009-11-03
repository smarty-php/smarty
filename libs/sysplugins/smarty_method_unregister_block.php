<?php

/**
* Smarty method Unregister_Block
* 
* Unregister a Smarty block function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a Smarty block function plugin
*/

/**
* Unregisters block function
* 
* @param string $block_tag name of template function
*/
function  Smarty_Method_Unregister_Block($smarty, $block_tag)
{
    if (isset($smarty->registered_plugins[$block_tag]) && $smarty->registered_plugins[$block_tag][0] == 'block') {
        unset($smarty->registered_plugins[$block_tag]);
    } 
} 

?>

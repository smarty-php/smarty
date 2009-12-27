<?php

/**
* Smarty method Register_Modifier
* 
* Registers a PHP function as Smarty modifier plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers modifier to be used in templates
* 
* @param object $smarty 
* @param string $modifier name of template modifier
* @param string $modifier_impl name of PHP function to register
*/
function  Smarty_Method_Register_Modifier($smarty, $modifier, $modifier_impl)
{
    if (isset($smarty->registered_plugins['modifier'][$modifier])) {
        throw new Exception("Plugin \"{$modifier}\" already registered");
    } elseif (!is_callable($modifier_impl)) {
        throw new Exception("Plugin \"{$modifier}\" not callable");
    } else {
        $smarty->registered_plugins['modifier'][$modifier] =
        array($modifier_impl);
    } 
} 
?>

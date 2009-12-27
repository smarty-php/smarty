<?php

/**
* Smarty method Register_Function
* 
* Registers a PHP function as Smarty function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers custom function to be used in templates
* 
* @param object $smarty 
* @param string $function_tag the name of the template function
* @param string $function_impl the name of the PHP function to register
* @param boolean $cacheable if true (default) this fuction is cachable
*/
function  Smarty_Method_Register_Function($smarty, $function_tag, $function_impl, $cacheable = true, $cache_attr = array())
{
    if (isset($smarty->registered_plugins['function'][$function_tag])) {
        throw new Exception("Plugin tag \"{$function_tag}\" already registered");
    } elseif (!is_callable($function_impl)) {
        throw new Exception("Plugin \"{$function_tag}\" not callable");
    } else {
        $smarty->registered_plugins['function'][$function_tag] =
        array($function_impl, $cacheable, $cache_attr);
    } 
} 

?>

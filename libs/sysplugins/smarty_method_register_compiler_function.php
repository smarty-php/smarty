<?php

/**
* Smarty method Register_Compiler_Function
* 
* Registers a PHP function as Smarty compiler function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Register a PHP function as Smarty compiler function plugin
*/

/**
* Registers compiler function
* 
* @param string $compiler_tag of template function
* @param string $compiler_impl name of PHP function to register
*/
function  Smarty_Method_Register_Compiler_Function($smarty, $compiler_tag, $compiler_impl, $cacheable = true)
{
    if (isset($smarty->registered_plugins['compiler'][$compiler_tag])) {
        throw new Exception("Plugin tag \"{$compiler_tag}\" already registered");
    } elseif (!is_callable($compiler_impl)) {
        throw new Exception("Plugin \"{$compiler_tag}\" not callable");
    } else {
        $smarty->registered_plugins['compiler'][$compiler_tag] =
        array($compiler_impl, $cacheable);
    } 
} 

?>

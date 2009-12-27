<?php

/**
* Smarty method Register_Block
* 
* Registers a PHP function as Smarty block function plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Register a PHP function as Smarty block function plugin
*/

/**
* Registers block function to be used in templates
* 
* @param string $block_tag name of template block
* @param string $block_impl PHP function to register
* @param boolean $cacheable if true (default) this fuction is cachable
*/
function  Smarty_Method_Register_Block($smarty, $block_tag, $block_impl, $cacheable = true, $cache_attr = array())
{
    if (isset($smarty->registered_plugins['block'][$block_tag])) {
        throw new Exception("Plugin tag \"{$block_tag}\" already registered");
    } elseif (!is_callable($block_impl)) {
        throw new Exception("Plugin \"{$block_tag}\" not callable");
    } else {
        $smarty->registered_plugins['block'][$block_tag] =
        array($block_impl, $cacheable, $cache_attr);
    } 
} 

?>

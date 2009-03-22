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
* Smarty class Register_Block
* 
* Register a PHP function as Smarty block function plugin
*/

class Smarty_Method_Register_Block extends Smarty_Internal_Base {
    /**
    * Registers block function to be used in templates
    * 
    * @param string $block_tag name of template block
    * @param string $block_impl PHP function to register
    * @param boolean $cacheable if true (default) this fuction is cachable
    */
    public function execute($block_tag, $block_impl, $cacheable = true)
    {
        if (isset($this->smarty->registered_plugins[$block_tag])) {
            throw new Exception("Plugin tag \"{$block_tag}\" already registered");
        } elseif (!is_callable($block_impl)) {
            throw new Exception("Plugin \"{$block_tag}\" not callable");
        } else {
            $this->smarty->registered_plugins[$block_tag] =
            array('block', $block_impl, $cacheable);
        } 
    } 
} 

?>

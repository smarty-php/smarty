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
* Smarty class Register_Compiler_Function
* 
* Register a PHP function as Smarty compiler function plugin
*/

class Smarty_Method_Register_Compiler_Function extends Smarty_Internal_Base {
    /**
    * Registers compiler function
    * 
    * @param string $compiler_tag of template function
    * @param string $compiler_impl name of PHP function to register
    */
    public function execute($compiler_tag, $compiler_impl, $cacheable = true)
    {
        if (isset($this->smarty->registered_plugins[$compiler_tag])) {
            throw new Exception("Plugin tag \"{$compiler_tag}\" already registered");
        } elseif (!is_callable($compiler_impl)) {
            throw new Exception("Plugin \"{$compiler_tag}\" not callable");
        } else {
            $this->smarty->registered_plugins[$compiler_tag] =
            array('compiler', $compiler_impl, $cacheable);
        } 
    } 
} 

?>

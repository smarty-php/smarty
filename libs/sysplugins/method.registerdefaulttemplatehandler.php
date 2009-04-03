<?php

/**
* Smarty method registerDefaultTemplateHandler
* 
* Registers a default template handler
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class registerDefaultTemplateHandler
* 
* Registers a default template handler
*/

class Smarty_Method_registerDefaultTemplateHandler extends Smarty_Internal_Base {
    /**
    * Registers a default template handler
    * 
    * @param string $ |array $plugin class/methode name
    */
    public function execute($plugin)
    {
        if (is_callable($plugin)) {
            $this->smarty->default_template_handler_func = $plugin;
        } else {
            throw new Exception("Default template handler not callable");
        } 
    } 
} 

?>

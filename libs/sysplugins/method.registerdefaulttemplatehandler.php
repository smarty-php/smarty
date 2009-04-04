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
    * @param string $ |array $function class/methode name
    */
    public function execute($function)
    {
        if (is_callable($function)) {
            $this->smarty->default_template_handler_func = $function;
        } else {
            throw new Exception('Default template handler "'.$function.'" not callable');
        } 
    } 
} 

?>

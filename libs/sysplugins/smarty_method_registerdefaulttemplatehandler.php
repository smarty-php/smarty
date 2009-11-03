<?php

/**
* Smarty method RegisterDefaultTemplateHandler
* 
* Registers a default template handler
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers a default template handler
* 
* @param object $smarty 
* @param string $ |array $function class/methode name
*/
function  Smarty_Method_RegisterDefaultTemplateHandler($smarty, $function)
{
    if (is_callable($function)) {
        $smarty->default_template_handler_func = $function;
    } else {
        throw new Exception('Default template handler "' . $function . '" not callable');
    } 
} 

?>

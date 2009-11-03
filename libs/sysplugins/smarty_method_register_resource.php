<?php

/**
* Smarty method Register_Resource
* 
* Registers a Smarty template resource
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Registers a resource to fetch a template
* 
* @param object $smarty 
* @param string $type name of resource
* @param array $functions array of functions to handle resource
*/
function  Smarty_Method_Register_Resource($smarty, $type, $functions)
{
    if (count($functions) == 4) {
        $smarty->_plugins['resource'][$type] =
        array($functions, false);
    } elseif (count($functions) == 5) {
        $smarty->_plugins['resource'][$type] =
        array(array(array(&$functions[0], $functions[1]) , array(&$functions[0], $functions[2]) , array(&$functions[0], $functions[3]) , array(&$functions[0], $functions[4])) , false);
    } else {
        throw new Exception("malformed function-list for '$type' in register_resource");
    } 
} 

?>

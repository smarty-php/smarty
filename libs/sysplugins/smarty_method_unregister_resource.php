<?php

/**
* Smarty method Unregister_Resource
* 
* Unregister a template resource
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Unregister a template resource
*/

/**
* Unregisters a resource
* 
* @param string $type name of resource
*/
function  Smarty_Method_Unregister_Resource($smarty, $type)
{
    unset($smarty->plugins['resource'][$type]);
} 

?>

<?php

/**
* Smarty method Get_Registered_Object
* 
* Registers a PHP object
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Returns a reference to a registered object
*/

/**
* return a reference to a registered object
* 
* @param string $name 
* @return object 
*/
function  Smarty_Method_Get_Registered_Object($smarty, $name)
{
    if (!isset($smarty->registered_objects[$name]))
        throw new Exception("'$name' is not a registered object");

    if (!is_object($smarty->registered_objects[$name][0]))
        throw new Exception("registered '$name' is not an object");

    return $smarty->registered_objects[$name][0];
} 

?>

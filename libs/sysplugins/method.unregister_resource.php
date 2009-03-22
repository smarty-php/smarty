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
* Smarty class Unregister_Resource
* 
* Unregister a template resource
*/

class Smarty_Method_Unregister_Resource extends Smarty_Internal_Base {
    /**
    * Unregisters a resource
    * 
    * @param string $type name of resource
    */
    public function execute($type)
    {
        unset($this->smarty->plugins['resource'][$type]);
    } 
} 

?>

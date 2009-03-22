<?php

/**
* Smarty method Unregister_Object
* 
* Unregister a PHP object
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Unregister_Object
* 
* Unregister a PHP object
*/

class Smarty_Method_Unregister_Object extends Smarty_Internal_Base {
    /**
    * Unregisters object
    * 
    * @param string $object name of template object
    */
    public function execute($object)
    {
        unset($this->smarty->registered_objects[$object]);
    } 
} 

?>

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
* Smarty class Get_Registered_Object
* 
* Returns a reference to a registered object
*/

class Smarty_Method_Get_Registered_Object extends Smarty_Internal_Base {
    /**
     * return a reference to a registered object
     *
     * @param string $name
     * @return object
     */
    public function execute($name) {
        if (!isset($this->smarty->registered_objects[$name]))
        throw new Exception("'$name' is not a registered object");

        if (!is_object($this->smarty->registered_objects[$name][0]))
        throw new Exception("registered '$name' is not an object");

        return $this->smarty->registered_objects[$name][0];
    }
} 

?>

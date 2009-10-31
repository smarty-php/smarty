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
* 
* Unregister a PHP object
*/

    /**
    * Unregisters object
    * 
    * @param string $object name of template object
    */
     function unregister_object($smarty, $object)
    {
        unset($smarty->registered_objects[$object]);
    } 

?>

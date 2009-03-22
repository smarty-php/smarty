<?php

/**
* Smarty method Unregister_Outputfilter
* 
* Unregister a outputfilter
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class Unregister_Outputfilter
* 
* Unregister a outputfilter
*/

class Smarty_Method_Unregister_Outputfilter extends Smarty_Internal_Base {
    /**
    * Registers an output filter function to apply
    * to a template output
    * 
    * @param callback $function 
    */
    public function execute($function)
    {
        unset($this->smarty->registered_filters['output'][$function]);
    } 
} 

?>

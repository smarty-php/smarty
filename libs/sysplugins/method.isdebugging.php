<?php

/**
* Smarty method isDebugging
* 
* is debugging
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isDebugging
* 
* is debugging
*/
class Smarty_Method_isDebugging extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->debugging;
   } 
} 

?>

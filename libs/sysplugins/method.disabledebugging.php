<?php

/**
* Smarty method disableDebugging
* 
* Disable debugging
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableDebugging
* 
* Disable debugging
*/
class Smarty_Method_DisableDebugging extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->debugging = false;
        return;
    } 
} 

?>

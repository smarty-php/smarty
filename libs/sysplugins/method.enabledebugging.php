<?php

/**
* Smarty method enableDebugging
* 
* Enable debugging
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableDebugging
* 
* Enable debugging
*/
class Smarty_Method_EnableDebugging extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->debugging = true;
        return;
    } 
} 

?>

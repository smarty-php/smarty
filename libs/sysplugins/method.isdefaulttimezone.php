<?php

/**
* Smarty method isDefaultTimezone
* 
* is setting of default timezone
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class isDefaultTimezone
* 
* is setting of default timezone
*/
class Smarty_Method_isDefaultTimezone extends Smarty_Internal_Base {
    public function execute()
    {
        return $this->smarty->set_timezone = false;
    } 
} 

?>

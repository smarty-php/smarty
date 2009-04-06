<?php

/**
* Smarty method disableDefaultTimezone
* 
* Disable setting of default timezone
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableDefaultTimezone
* 
* Disable setting of default timezone
*/
class Smarty_Method_disableDefaultTimezone extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->set_timezone = false;
        return;
    } 
} 

?>

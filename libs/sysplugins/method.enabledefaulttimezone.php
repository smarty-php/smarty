<?php

/**
* Smarty method enableDefaultTimezone
* 
* Enable setting of default timezone
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableDefaultTimezone
* 
* Enable setting of default timezone
*/
class Smarty_Method_enableDefaultTimezone extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->set_timezone = true;
        return;
    } 
} 

?>

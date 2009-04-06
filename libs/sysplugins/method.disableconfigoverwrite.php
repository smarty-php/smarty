<?php

/**
* Smarty method disableConfigOverwrite
* 
* Disable config overwrite mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class disableConfigOverwrite
* 
* Disable config overwrite mode
*/
class Smarty_Method_disableConfigOverwrite extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->config_overwrite = false;
        return ;
    } 
} 

?>

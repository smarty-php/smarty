<?php

/**
* Smarty method enableConfigOverwrite
* 
* Enable config overwrite mode
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class enableConfigOverwrite
* 
* Enable config overwrite mode
*/
class Smarty_Method_enableConfigOverwrite extends Smarty_Internal_Base {
    public function execute()
    {
        $this->smarty->config_overwrite = true;
        return;
    } 
} 

?>

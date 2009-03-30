<?php

/**
* Smarty method getConfigDir
* 
* Returns directory of config files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getConfigDir
* 
* Returns directory of config files
*/

class Smarty_Method_GetConfigDir extends Smarty_Internal_Base {
    /**
    * Returns directory of config files
    * 
    * @return array config folder
    */
    public function execute()
    {
        return $this->smarty->config_dir;
    } 
} 

?>

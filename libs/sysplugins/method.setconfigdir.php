<?php

/**
* Smarty method setConfigDir
* 
* Sets directory of config files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class setConfigDir
* 
* Sets directory of config files
*/

class Smarty_Method_SetConfigDir extends Smarty_Internal_Base {
    /**
    * Sets directory of config files
    * 
    * @param string $ config folder
    * @return 
    */
    public function execute($config_dir)
    {
        $this->smarty->config_dir = $config_dir;
        return;
    } 
} 

?>

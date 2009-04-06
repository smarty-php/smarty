<?php

/**
* Smarty method setPluginsDir
* 
* Sets directory of plugin files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class setPluginsDir
* 
* Sets directory of plugin files
*/

class Smarty_Method_SetPluginsDir extends Smarty_Internal_Base {
    /**
    * Sets directory of plugin files
    * 
    * @param string $ plugins folder
    * @return 
    */
    public function execute($plugins_dir)
    {
        $this->smarty->plugins_dir = (array)$plugins_dir;
        return;
    } 
} 

?>

<?php

/**
* Smarty method addPluginsDir
* 
* Adds directory of plugin files
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class addPluginsDir
* 
* Adds directory of plugin files
*/

class Smarty_Method_AddPluginsDir extends Smarty_Internal_Base {
    /**
    * Adds directory of plugin files
    * 
    * @param string|array $ plugins folder
    * @return 
    */
    public function execute($plugins_dir)
    {
        $this->smarty->plugins_dir = array_merge((array)$this->smarty->plugins_dir, (array)$plugins_dir);
        $this->smarty->plugins_dir = array_unique($this->smarty->plugins_dir);
        return;
    } 
} 

?>

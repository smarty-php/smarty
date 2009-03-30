<?php

/**
* Smarty method getPluginsDir
* 
* Returns directory of plugins
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* Smarty class getPluginsDir
* 
* Returns directory of plugins
*/

class Smarty_Method_GetPluginsDir extends Smarty_Internal_Base {
    /**
    * Returns directory of plugins
    * 
    * @return array plugins folder
    */
    public function execute()
    {
        return $this->smarty->plugins_dir;
    } 
} 

?>

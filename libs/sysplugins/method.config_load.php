<?php

/**
* Smarty methode Config_Load
* 
* Loads a config file
* 
* @package Smarty
* @subpackage Config
* @author Uwe Tews 
*/

/**
* Smarty class Config_Load
* 
* Load config file
*/

class Smarty_Method_Config_Load extends Smarty_Internal_Base {
    /**
    * load a config file optionally load just selected sections
    * 
    * @param string $config_file filename
    * @param mixed $sections array of section names, single section or null
    */
    function execute($config_file, $sections = null)
    {
        // load Config class
        $this->smarty->loadPlugin('Smarty_Internal_Config');
        $config = new Smarty_Internal_Config($config_file);
        $config->loadConfigVars($sections, $this->smarty);
    } 
} 

?>

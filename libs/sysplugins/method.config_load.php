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
* load a config file optionally load just selected sections
* 
* @param object $smarty 
* @param string $config_file filename
* @param mixed $sections array of section names, single section or null
*/
function config_load($smarty, $config_file, $sections = null)
{ 
    // load Config class
    $smarty->loadPlugin('Smarty_Internal_Config');
    $config = new Smarty_Internal_Config($config_file, $smarty);
    $config->loadConfigVars($sections, $smarty);
} 

?>

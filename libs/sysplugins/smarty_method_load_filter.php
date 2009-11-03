<?php

/**
* Smarty method Load_Filter
* 
* Loads a filter plugin
* 
* @package Smarty
* @subpackage SmartyMethod
* @author Uwe Tews 
*/

/**
* load a filter of specified type and name
* 
* @param string $type filter type
* @param string $name filter name
*/
function  Smarty_Method_Load_Filter($smarty, $type, $name)
{
    $_plugin = "smarty_{$type}filter_{$name}";
    $_filter_name = $_plugin;
    if ($smarty->loadPlugin($_plugin)) {
        if (class_exists($_plugin, false)) {
            $_plugin = array($_plugin, 'execute');
        } 
        if (is_callable($_plugin)) {
            $smarty->registered_filters[$type][$_filter_name] = $_plugin;
            return;
        } 
    } 
    throw new Exception("{$type}filter \"{$name}\" not callable");
} 

?>

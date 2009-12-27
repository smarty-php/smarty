<?php

/**
* Smarty Internal Plugin Filter Handler
* 
* Smarty filter handler class
* 
* @package Smarty
* @subpackage PluginsInternal
* @author Uwe Tews 
*/

/**
* Class for filter processing
*/
class Smarty_Internal_Filter_Handler {
    /**
    * Run filters over content
    * 
    * The filters will be lazy loaded if required
    * class name format: Smarty_FilterType_FilterName
    * plugin filename format: filtertype.filtername.php
    * Smarty2 filter plugins could be used
    * 
    * @param string $type the type of filter ('pre','post','output' or 'variable') which shall run
    * @param string $content the content which shall be processed by the filters
    * @return string the filtered content
    */
    static function runFilter($type, $content, $smarty, $flag = null)
    {
        $output = $content;
        if ($type != 'variable' || ($smarty->variable_filter && $flag !== false) || $flag === true) {
            // loop over autoload filters of specified type
            if (!empty($smarty->autoload_filters[$type])) {
                foreach ((array)$smarty->autoload_filters[$type] as $name) {
                    $plugin_name = "Smarty_{$type}filter_{$name}";
                    if ($smarty->loadPlugin($plugin_name)) {
                        // use class plugin if found
                        if (class_exists($plugin_name, false)) {
                            // loaded class of filter plugin
                            $output = call_user_func_array(array($plugin_name, 'execute'), array($output, $smarty));
                        } elseif (function_exists($plugin_name)) {
                            // use loaded Smarty2 style plugin
                            $output = call_user_func_array($plugin_name, array($output, $smarty));
                        } 
                    } else {
                        // nothing found, throw exception
                        throw new Exception("Unable to load filter {$plugin_name}");
                    } 
                } 
            } 
            // loop over registerd filters of specified type
            if (!empty($smarty->registered_filters[$type])) {
                foreach ($smarty->registered_filters[$type] as $key => $name) {
                    $output = call_user_func_array($smarty->registered_filters[$type][$key], array($output, $smarty));
                } 
            } 
        } 
        // return filtered output
        return $output;
    } 
} 

?>

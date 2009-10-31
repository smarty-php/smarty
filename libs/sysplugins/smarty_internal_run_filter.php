<?php

/**
* Smarty Internal Plugin Run Filter
* 
* Smarty run filter class
* 
* @package Smarty
* @subpackage PluginsInternal
* @author Uwe Tews 
*/

/**
* Class for filter processing
*/
class Smarty_Internal_Run_Filter {
    function __construct($smarty)
    {
        $this->smarty = $smarty;
    } 
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
    public function execute($type, $content, $flag = null)
    {
        $output = $content;
        if ($type != 'variable' || ($this->smarty->variable_filter && $flag !== false) || $flag === true) {
            // loop over autoload filters of specified type
            if (!empty($this->smarty->autoload_filters[$type])) {
                foreach ((array)$this->smarty->autoload_filters[$type] as $name) {
                    $plugin_name = "Smarty_{$type}filter_{$name}";
                    if ($this->smarty->loadPlugin($plugin_name)) {
                        // use class plugin if found
                        if (class_exists($plugin_name, false)) {
                            // loaded class of filter plugin
                            $output = call_user_func_array(array($plugin_name, 'execute'), array($output, $this->smarty));
                        } elseif (function_exists($plugin_name)) {
                            // use loaded Smarty2 style plugin
                            $output = call_user_func_array($plugin_name, array($output, $this->smarty));
                        } 
                    } else {
                        // nothing found, throw exception
                        throw new Exception("Unable to load filter {$plugin_name}");
                    } 
                } 
            } 
            // loop over registerd filters of specified type
            if (!empty($this->smarty->registered_filters[$type])) {
                foreach ($this->smarty->registered_filters[$type] as $key => $name) {
                    $output = call_user_func_array($this->smarty->registered_filters[$type][$key], array($output, $this->smarty));
                } 
            } 
        } 
        // return filtered output
        return $output;
    } 
} 

?>

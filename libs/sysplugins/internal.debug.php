<?php

/**
* Smarty Internal Plugin Debug
* 
* Class to collect data for the Smarty Debugging Consol
* 
* @package Smarty
* @subpackage Debug
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Debug Class
*/
class Smarty_Internal_Debug extends Smarty_Internal_TemplateBase {
    /**
    * Opens a window for the Smarty Debugging Consol and display the data
    */
    public static function display_debug($smarty)
    {
        // get template names
        $i = 0;
        $_template_data = array();
        if (is_array($smarty->template_objects)) {
            foreach ($smarty->template_objects as $_template_obj) {
                // exclude the debugging template from displayed data
                if ($smarty->debug_tpl != $_template_obj->resource_name) {
                    $_template_data[$i]['name'] = $_template_obj->getTemplateFilepath();
                    $_template_data[$i]['compile_time'] = $_template_obj->compile_time;
                    $_template_data[$i]['render_time'] = $_template_obj->render_time;
                    $_template_data[$i]['cache_time'] = $_template_obj->cache_time;
                    $i++;
                    if (false && $i == 1) {
                        foreach ($_template_obj->properties['file_dependency'] as $_file) {
                            $_template_data[$i]['name'] = $_file[0];
                            $_template_data[$i]['compile_time'] = 0;
                            $_template_data[$i]['render_time'] = 0;
                            $_template_data[$i]['cache_time'] = 0;
                            $i++;
                        } 
                    } 
                } 
            } 
        } 
        // prepare information of assigned variables
        $_assigned_vars = $smarty->tpl_vars;
        ksort($_assigned_vars);
        $_config_vars = $smarty->config_vars;
        ksort($_config_vars);
        $_template = new Smarty_Template ($smarty->debug_tpl, $smarty);
        $_template->caching = false;
        $_template->force_compile = false;
        $_template->security = false;
        $_template->assign('template_data', $_template_data);
        $_template->assign('assigned_vars', $_assigned_vars);
        $_template->assign('config_vars', $_config_vars);
        $_template->assign('execution_time', $smarty->_get_time() - $smarty->start_time);
        echo $smarty->fetch($_template);
    } 
} 

?>

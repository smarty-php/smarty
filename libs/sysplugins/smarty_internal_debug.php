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
class Smarty_Internal_Debug extends Smarty_Internal_Data {
    // template data
    static $template_data = array();

    /**
    * Start logging of compile time
    */
    public static function start_compile($template)
    {
        $key = self::get_key($template);
        self::$template_data[$key]['start_time'] = self::get_time();
    } 

    /**
    * End logging of compile time
    */
    public static function end_compile($template)
    {
        $key = self::get_key($template);
        self::$template_data[$key]['compile_time'] += self::get_time() - self::$template_data[$key]['start_time'];
    } 

    /**
    * Start logging of render time
    */
    public static function start_render($template)
    {
        $key = self::get_key($template);
        self::$template_data[$key]['start_time'] = self::get_time();
    } 

    /**
    * End logging of compile time
    */
    public static function end_render($template)
    {
        $key = self::get_key($template);
        self::$template_data[$key]['render_time'] += self::get_time() - self::$template_data[$key]['start_time'];
    } 

    /**
    * Start logging of cache time
    */
    public static function start_cache($template)
    {
        $key = self::get_key($template);
        self::$template_data[$key]['start_time'] = self::get_time();
    } 

    /**
    * End logging of cache time
    */
    public static function end_cache($template)
    {
        $key = self::get_key($template);
        self::$template_data[$key]['cache_time'] += self::get_time() - self::$template_data[$key]['start_time'];
    } 
    /**
    * Opens a window for the Smarty Debugging Consol and display the data
    */
    public static function display_debug($smarty)
    { 
        // prepare information of assigned variables
        $_assigned_vars = $smarty->tpl_vars;
        ksort($_assigned_vars);
        $_config_vars = $smarty->config_vars;
        ksort($_config_vars);
        $_template = new Smarty_Template ($smarty->debug_tpl, $smarty);
        $_template->caching = false;
        $_template->force_compile = false;
        $_template->security = false;
        $_template->cache_id = null;
        $_template->compile_id = null;
        $_template->assign('template_data', self::$template_data);
        $_template->assign('assigned_vars', $_assigned_vars);
        $_template->assign('config_vars', $_config_vars);
        $_template->assign('execution_time', $smarty->_get_time() - $smarty->start_time);
        echo $smarty->fetch($_template);
    } 

    /**
    * get_key
    */
    static function get_key($template)
    {
        // calculate Uid if not already done
        if ($template->templateUid == '') {
            $template->getTemplateFilepath();
        } 
        $key = $template->templateUid;
        if (isset(self::$template_data[$key])) {
            return $key;
        } else {
            self::$template_data[$key]['name'] = $template->getTemplateFilepath();
            self::$template_data[$key]['compile_time'] = 0;
            self::$template_data[$key]['render_time'] = 0;
            self::$template_data[$key]['cache_time'] = 0;
            return $key;
        } 
    } 

    /**
    * return current time
    * 
    * @returns double current time
    */
    static function get_time()
    {
        $_mtime = microtime();
        $_mtime = explode(" ", $_mtime);
        return (double)($_mtime[1]) + (double)($_mtime[0]);
    } 
} 

?>

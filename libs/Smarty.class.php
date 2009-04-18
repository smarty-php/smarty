<?php

/**
* Project:     Smarty: the PHP compiling template engine
* File:        Smarty.class.php
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
* 
* For questions, help, comments, discussion, etc., please join the
* Smarty mailing list. Send a blank e-mail to
* smarty-discussion-subscribe@googlegroups.com
* 
* @link http://www.smarty.net/
* @copyright 2008 New Digital Group, Inc.
* @author Monte Ohrt <monte at ohrt dot com> 
* @author Uwe Tews 
* @package Smarty
* @version 3.0-alpha1
*/

/**
* set SMARTY_DIR to absolute path to Smarty library files.
* if not defined, include_path will be used. Sets SMARTY_DIR only if user
* application has not already defined it.
*/
if (!defined('SMARTY_DIR')) {
    define('SMARTY_DIR', dirname(__FILE__) . DIRECTORY_SEPARATOR);
} 

/**
* define variable scopes
*/
define('SMARTY_LOCAL_SCOPE', 0);
define('SMARTY_PARENT_SCOPE', 1);
define('SMARTY_ROOT_SCOPE', 2);
define('SMARTY_GLOBAL_SCOPE', 3);

/**
* load required base class for creation of the smarty object
*/
require_once(dirname(__FILE__) . DIRECTORY_SEPARATOR . 'sysplugins' . DIRECTORY_SEPARATOR . 'internal.templatebase.php');

/**
* This is the main Smarty class
*/
class Smarty extends Smarty_Internal_TemplateBase {
    // smarty version
    static $_version = 'Smarty3Alpha'; 
    // class used for templates
    public $template_class = 'Smarty_Internal_Template'; 
    // display error on not assigned variabled
    static $error_unassigned = false; 
    // template directory
    public $template_dir = null; 
    // default template handler
    public $default_template_handler_func = null; 
    // compile directory
    public $compile_dir = null; 
    // plugins directory
    public $plugins_dir = null; 
    // cache directory
    public $cache_dir = null; 
    // config directory
    public $config_dir = null; 
    // force template compiling?
    public $force_compile = false; 
    // check template for modifications?
    public $compile_check = true; 
    // use sub dirs for compiled/cached files?
    public $use_sub_dirs = false; 
    // php file extention
    public $php_ext = '.php'; 
    // compile_error?
    public $compile_error = false; 
    // caching enabled
    public $caching = false; 
    // caching lifetime
    public $caching_lifetime = 0; 
    // cache_id
    public $cache_id = null; 
    // compile_id
    public $compile_id = null; 
    // template delimiters
    public $left_delimiter = "{";
    public $right_delimiter = "}"; 
    // security
    public $security = false;
    public $security_policy = null;
    public $security_handler = null;
    public $direct_access_security = true; 
    // debug mode
    public $debugging = false;
    public $debugging_ctrl = 'URL';
    public $smarty_debug_id = 'SMARTY_DEBUG';
    public $debug_tpl = null;
    public $request_use_auto_globals = true; 
    // When set, smarty does uses this value as error_reporting-level.
    public $error_reporting = null; 
    // config var settings
    public $config_overwrite = true; //Controls whether variables with the same name overwrite each other.
    public $config_booleanize = true; //Controls whether config values of on/true/yes and off/false/no get converted to boolean
    public $config_read_hidden = true; //Controls whether hidden config sections/vars are read from the file.          
    // config vars
    public $config_vars = array(); 
    // assigned tpl vars
    public $tpl_vars = array(); 
    // assigned global tpl vars
    public $global_tpl_vars = array(); 
    // dummy parent object
    public $parent = null; 
    // system plugins directory
    private $sysplugins_dir = null; 
    // resource type used if none given
    public $default_resource_type = 'file'; 
    // charset of template
    public $resource_char_set = 'UTF-8'; 
    // caching type
    public $default_caching_type = 'file'; 
    // internal cache resource types
    public $cache_resorce_types = array('file'); 
    // config type
    public $default_config_type = 'file'; 
    // class used for cacher
    public $cacher_class = 'Smarty_Internal_Cacher_InlineCode'; 
    // exception handler: set null to disable
    public $exception_handler = array('SmartyException', 'getStaticException'); 
    // cached template objects
    static $template_objects = null; 
    // check If-Modified-Since headers
    public $cache_modified_check = false; 
    // registered plugins
    public $registered_plugins = array(); 
    // plugin search order
    public $plugin_search_order = array('function', 'block', 'compiler', 'class'); 
    // plugin handler object
    public $plugin_handler = null; 
    // default plugin handler
    public $default_plugin_handler_func = null; 
    // registered objects
    public $registered_objects = array(); 
    // registered filters
    public $registered_filters = array(); 
    // filter handler
    public $filter_handler = null; 
    // autoload filter
    public $autoload_filters = array(); 
    // status of filter on variable output
    public $variable_filter = true; 
    // cache resorce objects
    public $cache_resource_objects = array(); 
    // write file object
    public $write_file_object = null; 
    // global internal smarty  vars
    public $_smarty_vars = array(); 
    // start time for execution time calculation
    public $start_time = 0; 
    // has multibyte string functions?
    public $has_mb = false;
    /**
    * Class constructor, initializes basic smarty properties
    */
    public function __construct()
    {
        // set instance object
        self::instance($this); 

        if (is_callable('mb_internal_encoding')) {
            $this->has_mb = true;
            mb_internal_encoding($this->resource_char_set);
        } 
        if (function_exists("date_default_timezone_set")) {
            date_default_timezone_set(date_default_timezone_get());
        } 
        $this->start_time = $this->_get_time(); 
        // set exception handler
        if (!empty($this->exception_handler))
            set_exception_handler($this->exception_handler); 
        // set default dirs
        $this->template_dir = array('.' . DIRECTORY_SEPARATOR . 'templates' . DIRECTORY_SEPARATOR);
        $this->compile_dir = '.' . DIRECTORY_SEPARATOR . 'templates_c' . DIRECTORY_SEPARATOR;
        $this->plugins_dir = array(dirname(__FILE__) . DIRECTORY_SEPARATOR . 'plugins' . DIRECTORY_SEPARATOR);
        $this->cache_dir = '.' . DIRECTORY_SEPARATOR . 'cache' . DIRECTORY_SEPARATOR;
        $this->config_dir = '.' . DIRECTORY_SEPARATOR . 'configs' . DIRECTORY_SEPARATOR;
        $this->sysplugins_dir = dirname(__FILE__) . DIRECTORY_SEPARATOR . 'sysplugins' . DIRECTORY_SEPARATOR;
        $this->debug_tpl = SMARTY_DIR . 'debug.tpl'; 
        // load base plugins
        $this->loadPlugin('Smarty_Internal_Base');
        $this->loadPlugin('Smarty_Internal_PluginBase');
        $this->loadPlugin($this->template_class);
        $this->loadPlugin('Smarty_Internal_Plugin_Handler');
        $this->plugin_handler = new Smarty_Internal_Plugin_Handler;
        $this->loadPlugin('Smarty_Internal_Run_Filter');
        $this->filter_handler = new Smarty_Internal_Run_Filter;
        if (!$this->debugging && $this->debugging_ctrl == 'URL') {
            $_query_string = $this->request_use_auto_globals ? isset($_SERVER['QUERY_STRING']) ? $_SERVER['QUERY_STRING']:'' : isset($GLOBALS['HTTP_SERVER_VARS']['QUERY_STRING']) ? $GLOBALS['HTTP_SERVER_VARS']['QUERY_STRING']:'';
            if (false !== strpos($_query_string, $this->smarty_debug_id)) {
                if (false !== strpos($_query_string, $this->smarty_debug_id . '=on')) {
                    // enable debugging for this browser session
                    setcookie('SMARTY_DEBUG', true);
                    $this->debugging = true;
                } elseif (false !== strpos($_query_string, $this->smarty_debug_id . '=off')) {
                    // disable debugging for this browser session
                    setcookie('SMARTY_DEBUG', false);
                    $this->debugging = false;
                } else {
                    // enable debugging for this page
                    $this->debugging = true;
                } 
            } else {
                if ($this->request_use_auto_globals && isset($_COOKIE['SMARTY_DEBUG'])) {
                    $this->debugging = true;
                } elseif (!$this->request_use_auto_globals && isset($GLOBALS['HTTP_COOKIE_VARS']['SMARTY_DEBUG'])) {
                    $this->debugging = true;
                } 
            } 
        } 
    } 

    /**
    * Class destructor
    */
    public function __destruct()
    { 
        // restore to previous exception handler, if any
        if (!empty($this->exception_handler))
            restore_exception_handler();
    } 

    /**
    * Sets a static instance of the smarty object. Retrieve with:
    * $smarty = Smarty::instance();
    * 
    * @param object $new_instance the Smarty object when setting
    * @return object reference to Smarty object
    */
    public static function &instance($new_instance = null)
    {
        static $instance = null;
        if (isset($new_instance) && is_object($new_instance))
            $instance = $new_instance;
        return $instance;
    } 

    /**
    * fetches a rendered Smarty template
    * 
    * @param string $template the resource handle of the template file or template object
    * @param object $ |null $parent next higher level of Smarty variables
    * @param mixed $cache_id cache id to be used with this template
    * @param mixed $compile_id compile id to be used with this template
    * @return string rendered template output
    */
    public function fetch($template, $parent = null, $cache_id = null, $compile_id = null)
    {
        if ($parent === null) {
            // get default Smarty data object
            $parent = $this;
        } 
        // create template object if necessary
        ($template instanceof $this->template_class)? $_template = $template :
        $_template = $this->createTemplate ($template, $parent , $cache_id, $compile_id);
        $_smarty_old_error_level = $this->debugging ? error_reporting() : error_reporting(isset($this->error_reporting)
            ? $this->error_reporting : error_reporting() &~E_NOTICE); 
        // return redered template
        $_output = $_template->getRenderedTemplate();
        $_template->rendered_content = null;
        error_reporting($_smarty_old_error_level);
        return $_output;
    } 

    /**
    * displays a Smarty template
    * 
    * @param string $ |object $template the resource handle of the template file  or template object
    * @param object $parent next higher level of Smarty variables
    * @param mixed $cache_id cache id to be used with this template
    * @param mixed $compile_id compile id to be used with this template
    */
    public function display($template, $parent = null, $cache_id = null, $compile_id = null)
    { 
        // display template
        echo $this->fetch ($template, $parent , $cache_id, $compile_id); 
        // debug output?
        if ($this->debugging) {
            $this->loadPlugin('Smarty_Internal_Debug');
            Smarty_Internal_Debug::display_debug();
        } 
        return true;
    } 

    /**
    * test if cache i valid
    * 
    * @param string $ |object $template the resource handle of the template file or template object
    * @param mixed $cache_id cache id to be used with this template
    * @param mixed $compile_id compile id to be used with this template
    * @return boolean cache status
    */
    public function is_cached($template, $cache_id = null, $compile_id = null)
    {
        if (!($template instanceof $this->template_class)) {
            $template = $this->createTemplate ($template, $this, $cache_id, $compile_id);
        } 
        // return cache status of template
        return $template->isCached();
    } 

    /**
    * Load the plugin with security definition and enables security
    * 
    * @param string $security_policy plugin to load
    */
    public function enableSecurity($security_policy = 'Smarty_SecurityPolicy_Default')
    {
        if ($this->loadPlugin($security_policy)) {
            if (!class_exists('Smarty_Security_Policy')) {
                throw new Exception("Security policy must define class 'Smarty_Security_Policy'");
            } 
            $this->security_policy = new Smarty_Security_Policy();
            $this->loadPlugin('Smarty_Internal_Security_Handler');
            $this->security_handler = new Smarty_Internal_Security_Handler();
            $this->security = true;
        } else {
            throw new Exception("Security policy {$security_policy} not found");
        } 
    } 

    /**
    * Set template directory
    * 
    * @param string $ |array $template_dir folder(s) of template sorces
    */
    public function setTemplateDir($template_dir)
    {
        $this->smarty->template_dir = (array)$template_dir;
        return;
    } 
    /**
    * Adds template directory(s) to existing ones
    * 
    * @param string $ |array $template_dir folder(s) of template sources
    */
    public function addTemplateDir($template_dir)
    {
        $this->smarty->template_dir = array_merge((array)$this->smarty->template_dir, (array)$template_dir);
        $this->smarty->template_dir = array_unique($this->smarty->template_dir);
        return;
    } 
    /**
    * Set compile directory
    * 
    * @param string $compile_dir folder of compiled template sources
    */
    public function setCompileDir($compile_dir)
    {
        $this->smarty->compile_dir = $compile_dir;
        return;
    } 
    /**
    * Set cache directory
    * 
    * @param string $cache_dir folder of cache files
    */
    public function setCacheDir($cache_dir)
    {
        $this->smarty->cache_dir = $cache_dir;
        return;
    } 
    /**
    * Enable Caching
    */
    public function enableCaching()
    {
        $this->smarty->caching = true;
        return;
    } 
    /**
    * Set caching life time
    * 
    * @param integer $lifetime lifetime of cached file in seconds
    */
    public function setCachingLifetime($lifetime)
    {
        $this->smarty->caching_lifetime = $lifetime;
        return;
    } 
    /**
    * Takes unknown classes and loads plugin files for them
    * class name format: Smarty_PluginType_PluginName
    * plugin filename format: plugintype.pluginname.php
    * 
    * @param string $plugin_name class plugin name to load
    * @return boolean 
    */
    public function loadPlugin($plugin_name)
    { 
        // if class exists, exit silently (already loaded)
        if (class_exists($plugin_name, false))
            return true; 
        // if callable as function, exit silently (already loaded)
        if (is_callable($plugin_name))
            return true; 
        // Plugin name is expected to be: Smarty_[Type]_[Name]
        $plugin_name = strtolower($plugin_name);
        $name_parts = explode('_', $plugin_name, 3); 
        // class name must have three parts to be valid plugin
        if (count($name_parts) < 3 || $name_parts[0] !== 'smarty') {
            throw new Exception("plugin {$plugin_name} is not a valid name format");
            return false;
        } 
        // plugin filename is expected to be: [type].[name].php
        $plugin_filename = "{$name_parts[1]}.{$name_parts[2]}{$this->php_ext}"; 
        // if type is "internal", get plugin from sysplugins
        if ($name_parts[1] == 'internal') {
            if (file_exists($this->sysplugins_dir . $plugin_filename)) {
                require_once($this->sysplugins_dir . $plugin_filename);
                return true;
            } else {
                return false;
            } 
        } 
        // loop through plugin dirs and find the plugin
        foreach((array)$this->plugins_dir as $plugin_dir) {
            if (file_exists($plugin_dir . $plugin_filename)) {
                require_once($plugin_dir . $plugin_filename);
                return true;
            } 
        } 
        // no plugin loaded
        return false;
    } 

    /**
    * Sets the exception handler for Smarty.
    * 
    * @param mixed $handler function name or array with object/method names
    * @return string previous exception handler
    */
    public function setExceptionHandler($handler)
    {
        $this->exception_handler = $handler;
        return set_exception_handler($handler);
    } 

    /**
    * Takes unknown class methods and lazy loads sysplugin files for them
    * class name format: Smarty_Method_MethodName
    * plugin filename format: method.methodname.php
    * 
    * @param string $name unknown methode name
    * @param array $args aurgument array
    */
    public function __call($name, $args)
    {
        $class_name = "Smarty_Method_{$name}";
        if (!class_exists($class_name, false)) {
            $plugin_filename = strtolower('method.' . $name . $this->php_ext);
            if (!file_exists($this->sysplugins_dir . $plugin_filename)) {
                throw new Exception("Sysplugin file " . $plugin_filename . " does not exist");
            } 
            require_once($this->sysplugins_dir . $plugin_filename);
            if (!class_exists($class_name, false)) {
                throw new Exception ("Sysplugin file " . $plugin_filename . " does not define class " . $class_name);
            } 
        } 
        $method = new $class_name;
        return call_user_func_array(array($method, 'execute'), $args);
    } 
} 

/**
* Smarty Exception Handler
* 
* All errors thrown in Smarty will be handled here.
* 
* @param string $message the error message
* @param string $code the error code
*/
class SmartyException {
    public static function printException($e)
    {
        echo "Code: " . $e->getCode() . "<br />Error: " . htmlentities($e->getMessage()) . "<br />"
         . "File: " . $e->getFile() . "<br />"
         . "Line: " . $e->getLine() . "<br />"
         . "\n";
    } 

    public static function getStaticException($e)
    {
        self::printException($e);
    } 
} 

?>

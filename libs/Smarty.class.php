<?php

/**
 * Project:     Smarty: the PHP compiling template engine
 * File:        Smarty.class.php
 * SVN:         $Id$
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
 * @version 3-SVN$Rev: 3286 $
 */

/**
 * define shorthand directory separator constant
 */
if (!defined('DS')) {
    define('DS', DIRECTORY_SEPARATOR);
} 

/**
 * set SMARTY_DIR to absolute path to Smarty library files.
 * Sets SMARTY_DIR only if user application has not already defined it.
 */
if (!defined('SMARTY_DIR')) {
    define('SMARTY_DIR', dirname(__FILE__) . DS);
} 

/**
 * set SMARTY_SYSPLUGINS_DIR to absolute path to Smarty internal plugins.
 * Sets SMARTY_SYSPLUGINS_DIR only if user application has not already defined it.
 */
if (!defined('SMARTY_SYSPLUGINS_DIR')) {
    define('SMARTY_SYSPLUGINS_DIR', SMARTY_DIR . 'sysplugins' . DS);
} 
if (!defined('SMARTY_PLUGINS_DIR')) {
    define('SMARTY_PLUGINS_DIR', SMARTY_DIR . 'plugins' . DS);
} 
if (!defined('SMARTY_RESOURCE_CHAR_SET')) {
    define('SMARTY_RESOURCE_CHAR_SET', 'UTF-8');
} 
if (!defined('SMARTY_RESOURCE_DATE_FORMAT')) {
    define('SMARTY_RESOURCE_DATE_FORMAT', '%b %e, %Y');
} 

/**
 * define variable scopes
 */
define('SMARTY_LOCAL_SCOPE', 0);
define('SMARTY_PARENT_SCOPE', 1);
define('SMARTY_ROOT_SCOPE', 2);
define('SMARTY_GLOBAL_SCOPE', 3);

/**
 * define caching modes
 */
define('SMARTY_CACHING_OFF', 0);
define('SMARTY_CACHING_LIFETIME_CURRENT', 1);
define('SMARTY_CACHING_LIFETIME_SAVED', 2);

/**
 * This determines how Smarty handles "<?php ... ?>" tags in templates.
 * possible values:
 */
define('SMARTY_PHP_PASSTHRU', 0); //-> print tags as plain text
define('SMARTY_PHP_QUOTE', 1); //-> escape tags as entities
define('SMARTY_PHP_REMOVE', 2); //-> escape tags as entities
define('SMARTY_PHP_ALLOW', 3); //-> escape tags as entities

/**
 * register the class autoloader
 */
if (!defined('SMARTY_SPL_AUTOLOAD')) {
    define('SMARTY_SPL_AUTOLOAD', 0);
} 

if (SMARTY_SPL_AUTOLOAD && set_include_path(get_include_path() . PATH_SEPARATOR . SMARTY_SYSPLUGINS_DIR) !== false) {
    $registeredAutoLoadFunctions = spl_autoload_functions();
    if (!isset($registeredAutoLoadFunctions['spl_autoload'])) {
        spl_autoload_register();
    } 
} else {
    spl_autoload_register('smartyAutoload');
} 

/**
 * This is the main Smarty class
 */
class Smarty extends Smarty_Internal_Data {
    // smarty version
    const SMARTY_VERSION = 'Smarty3-SVN$Rev: 3286 $'; 
    // auto literal on delimiters with whitspace
    public $auto_literal = true; 
    // display error on not assigned variables
    public $error_unassigned = false; 
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
    // locking concurrent compiles
    public $compile_locking = true; 
    // use sub dirs for compiled/cached files?
    public $use_sub_dirs = false; 
    // compile_error?
    public $compile_error = false; 
    // caching enabled
    public $caching = false; 
    // merge compiled includea
    public $merge_compiled_includes = false; 
    // cache lifetime
    public $cache_lifetime = 3600; 
    // force cache file creation
    public $force_cache = false; 
    // cache_id
    public $cache_id = null; 
    // compile_id
    public $compile_id = null; 
    // template delimiters
    public $left_delimiter = "{";
    public $right_delimiter = "}"; 
    // security
    public $security_class = 'Smarty_Security';
    public $php_handling = SMARTY_PHP_PASSTHRU;
    public $allow_php_tag = false;
    public $allow_php_templates = false;
    public $security = false;
    public $security_policy = null;
    public $security_handler = null;
    public $direct_access_security = true; 
    public $trusted_dir = array();
    // debug mode
    public $debugging = false;
    public $debugging_ctrl = 'NONE';
    public $smarty_debug_id = 'SMARTY_DEBUG';
    public $debug_tpl = null; 
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
    // global template functions
    public $template_functions = array(); 
    // resource type used if none given
    public $default_resource_type = 'file'; 
    // caching type
    public $caching_type = 'file'; 
    // internal cache resource types
    public $cache_resource_types = array('file'); 
    // internal cache resource objects
    public $cache_resource_objects = array(); 
    // internal config properties
    public $properties = array(); 
    // config type
    public $default_config_type = 'file'; 
    // cached template objects
    public $template_objects = null; 
    // check If-Modified-Since headers
    public $cache_modified_check = false; 
    // registered plugins
    public $registered_plugins = array(); 
    // plugin search order
    public $plugin_search_order = array('function', 'block', 'compiler', 'class'); 
    // registered objects
    public $registered_objects = array(); 
    // registered classes
    public $registered_classes = array(); 
    // registered filters
    public $registered_filters = array(); 
    // autoload filter
    public $autoload_filters = array(); 
    // status of filter on variable output
    public $variable_filter = true; 
    // default modifier
    public $default_modifiers = array(); 
    // global internal smarty  vars
    public $_smarty_vars = array(); 
    // start time for execution time calculation
    public $start_time = 0; 
    // default file permissions
    public $_file_perms = 0644; 
    // default dir permissions
    public $_dir_perms = 0771; 
    // smarty object reference
    public $smarty = null; 
    // block tag hierarchy
    public $_tag_stack = array(); 
    // flag if {block} tag is compiled for template inheritance
    public $inheritance = false;
    // plugins
    public $_plugins = array(); 
    // generate deprecated function call notices?
    public $deprecation_notices = true;

    /**
     * Class constructor, initializes basic smarty properties
     */
    public function __construct()
    { 
        // self reference needed by other classes methods
        $this->smarty = $this;

        if (is_callable('mb_internal_encoding')) {
            mb_internal_encoding(SMARTY_RESOURCE_CHAR_SET);
        } 
        $this->start_time = microtime(true); 
        // set default dirs
        $this->template_dir = array('.' . DS . 'templates' . DS);
        $this->compile_dir = '.' . DS . 'templates_c' . DS;
        $this->plugins_dir = array(SMARTY_PLUGINS_DIR);
        $this->cache_dir = '.' . DS . 'cache' . DS;
        $this->config_dir = '.' . DS . 'configs' . DS;
        $this->debug_tpl = SMARTY_DIR . 'debug.tpl';
        if (!$this->debugging && $this->debugging_ctrl == 'URL') {
            if (isset($_SERVER['QUERY_STRING'])) {
                $_query_string = $_SERVER['QUERY_STRING'];
            } else {
                $_query_string = '';
            } 
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
                if (isset($_COOKIE['SMARTY_DEBUG'])) {
                    $this->debugging = true;
                } 
            } 
        } 
        if (isset($_SERVER['SCRIPT_NAME'])) {
            $this->assignGlobal('SCRIPT_NAME', $_SERVER['SCRIPT_NAME']);
        } 
    } 

    /**
     * Class destructor
     */
    public function __destruct()
    { 
    } 

    /**
     * fetches a rendered Smarty template
     * 
     * @param string $template the resource handle of the template file or template object
     * @param mixed $cache_id cache id to be used with this template
     * @param mixed $compile_id compile id to be used with this template
     * @param object $ |null $parent next higher level of Smarty variables
     * @return string rendered template output
     */
    public function fetch($template, $cache_id = null, $compile_id = null, $parent = null, $display = false)
    {
        if (is_object($cache_id)) {
            $parent = $cache_id;
            $cache_id = null;
        } 
        if ($parent === null) {
            // get default Smarty data object
            $parent = $this;
        } 
        // create template object if necessary
        ($template instanceof $this->template_class)? $_template = $template :
        $_template = $this->createTemplate ($template, $cache_id, $compile_id, $parent);
        $_smarty_old_error_level = $this->debugging ? error_reporting() : error_reporting(isset($this->error_reporting)
            ? $this->error_reporting : error_reporting() &~E_NOTICE); 
        // obtain data for cache modified check
        if ($this->cache_modified_check && $this->caching && $display) {
            $_isCached = $_template->isCached() && !$_template->has_nocache_code;
            if ($_isCached) {
                $_gmt_mtime = gmdate('D, d M Y H:i:s', $_template->getCachedTimestamp()) . ' GMT';
            } else {
                $_gmt_mtime = '';
            } 
        } 
        // return redered template
        if (isset($this->autoload_filters['output']) || isset($this->registered_filters['output'])) {
            $_output = Smarty_Internal_Filter_Handler::runFilter('output', $_template->getRenderedTemplate(), $this, $_template);
        } else {
            $_output = $_template->getRenderedTemplate();
        } 
        $_template->rendered_content = null;
        error_reporting($_smarty_old_error_level); 
        // display or fetch
        if ($display) {
            if ($this->caching && $this->cache_modified_check) {
                $_last_modified_date = @substr($_SERVER['HTTP_IF_MODIFIED_SINCE'], 0, strpos($_SERVER['HTTP_IF_MODIFIED_SINCE'], 'GMT') + 3);
                if ($_isCached && $_gmt_mtime == $_last_modified_date) {
                    if (php_sapi_name() == 'cgi')
                        header('Status: 304 Not Modified');
                    else
                        header('HTTP/1.1 304 Not Modified');
                } else {
                    header('Last-Modified: ' . gmdate('D, d M Y H:i:s', $_template->getCachedTimestamp()) . ' GMT');
                    echo $_output;
                } 
            } else {
                echo $_output;
            } 
            // debug output
            if ($this->debugging) {
                Smarty_Internal_Debug::display_debug($this);
            } 
            return;
        } else {
            // return fetched content
            return $_output;
        } 
    } 

    /**
     * displays a Smarty template
     * 
     * @param string $ |object $template the resource handle of the template file  or template object
     * @param mixed $cache_id cache id to be used with this template
     * @param mixed $compile_id compile id to be used with this template
     * @param object $parent next higher level of Smarty variables
     */
    public function display($template, $cache_id = null, $compile_id = null, $parent = null)
    { 
        // display template
        $this->fetch ($template, $cache_id, $compile_id, $parent, true);
    } 

    /**
     * test if cache i valid
     * 
     * @param string $ |object $template the resource handle of the template file or template object
     * @param mixed $cache_id cache id to be used with this template
     * @param mixed $compile_id compile id to be used with this template
     * @return boolean cache status
     */
    public function isCached($template, $cache_id = null, $compile_id = null)
    {
        if (!($template instanceof $this->template_class)) {
            $template = $this->createTemplate ($template, $cache_id, $compile_id, $this);
        } 
        // return cache status of template
        return $template->isCached();
    } 

    /**
     * creates a data object
     * 
     * @param object $parent next higher level of Smarty variables
     * @returns object data object
     */
    public function createData($parent = null)
    {
        return new Smarty_Data($parent, $this);
    } 

    /**
     * creates a template object
     * 
     * @param string $template the resource handle of the template file
     * @param object $parent next higher level of Smarty variables
     * @param mixed $cache_id cache id to be used with this template
     * @param mixed $compile_id compile id to be used with this template
     * @returns object template object
     */
    public function createTemplate($template, $cache_id = null, $compile_id = null, $parent = null)
    {
        if (is_object($cache_id) || is_array($cache_id)) {
            $parent = $cache_id;
            $cache_id = null;
        } 
        if (is_array($parent)) {
            $data = $parent;
            $parent = null;
        } else {
            $data = null;
        } 
        if (!is_object($template)) {
            // we got a template resource
            // already in template cache?
            $_templateId = crc32($template . $cache_id . $compile_id);
            if (isset($this->template_objects[$_templateId]) && $this->caching) {
                // return cached template object
                $tpl = $this->template_objects[$_templateId];
            } else {
                // create new template object
                $tpl = new $this->template_class($template, $this, $parent, $cache_id, $compile_id);
            } 
        } else {
            // just return a copy of template class
            $tpl = $template;
        } 
        // fill data if present
        if (is_array($data)) {
            // set up variable values
            foreach ($data as $_key => $_val) {
                $tpl->tpl_vars[$_key] = new Smarty_variable($_val);
            } 
        } 
        return $tpl;
    } 

    /**
     * Loads security class and enables security
     */
    public function enableSecurity()
    {
        if (isset($this->security_class)) {
            $this->security_policy = new $this->security_class;
            $this->security_handler = new Smarty_Internal_Security_Handler($this);
            $this->security = true;
        } else {
            throw new SmartyException('Property security_class is not defined');
        } 
    } 

    /**
     * Disable security
     */
    public function disableSecurity()
    {
        $this->security = false;
    } 

    /**
     * Set template directory
     * 
     * @param string $ |array $template_dir folder(s) of template sorces
     */
    public function setTemplateDir($template_dir)
    {
        $this->template_dir = (array)$template_dir;
        return;
    } 

    /**
     * Adds template directory(s) to existing ones
     * 
     * @param string $ |array $template_dir folder(s) of template sources
     */
    public function addTemplateDir($template_dir)
    {
        $this->template_dir = array_merge((array)$this->template_dir, (array)$template_dir);
        $this->template_dir = array_unique($this->template_dir);
        return;
    } 

    /**
     * Check if a template resource exists
     * 
     * @param string $resource_name template name
     * @return boolean status
     */
    function templateExists($resource_name)
    { 
        // create template object
        $save = $this->template_objects;
        $tpl = new $this->template_class($resource_name, $this); 
        // check if it does exists
        $result = $tpl->isExisting();
        $this->template_objects = $save;
        unset ($tpl);
        return $result;
    } 

    /**
     * Takes unknown classes and loads plugin files for them
     * class name format: Smarty_PluginType_PluginName
     * plugin filename format: plugintype.pluginname.php
     * 
     * @param string $plugin_name class plugin name to load
     * @return string |boolean filepath of loaded file or false
     */
    public function loadPlugin($plugin_name, $check = true)
    { 
        // if function or class exists, exit silently (already loaded)
        if ($check && (is_callable($plugin_name) || class_exists($plugin_name, false)))
            return true; 
        // Plugin name is expected to be: Smarty_[Type]_[Name]
        $_plugin_name = strtolower($plugin_name);
        $_name_parts = explode('_', $_plugin_name, 3); 
        // class name must have three parts to be valid plugin
        if (count($_name_parts) < 3 || $_name_parts[0] !== 'smarty') {
            throw new SmartyException("plugin {$plugin_name} is not a valid name format");
            return false;
        } 
        // if type is "internal", get plugin from sysplugins
        if ($_name_parts[1] == 'internal') {
            $file = SMARTY_SYSPLUGINS_DIR . $_plugin_name . '.php';
            if (file_exists($file)) {
                require_once($file);
                return $file;
            } else {
                return false;
            } 
        } 
        // plugin filename is expected to be: [type].[name].php
        $_plugin_filename = "{$_name_parts[1]}.{$_name_parts[2]}.php"; 
        // loop through plugin dirs and find the plugin
        foreach((array)$this->plugins_dir as $_plugin_dir) {
            if (strpos('/\\', substr($_plugin_dir, -1)) === false) {
                $_plugin_dir .= DS;
            } 
            $file = $_plugin_dir . $_plugin_filename;
            if (file_exists($file)) {
                require_once($file);
                return $file;
            } 
        } 
        // no plugin loaded
        return false;
    } 

    /**
     * load a filter of specified type and name
     * 
     * @param string $type filter type
     * @param string $name filter name
     * @return bool 
     */
    function loadFilter($type, $name)
    {
        $_plugin = "smarty_{$type}filter_{$name}";
        $_filter_name = $_plugin;
        if ($this->loadPlugin($_plugin)) {
            if (class_exists($_plugin, false)) {
                $_plugin = array($_plugin, 'execute');
            } 
            if (is_callable($_plugin)) {
                return $this->registered_filters[$type][$_filter_name] = $_plugin;
            } 
        } 
        throw new SmartyException("{$type}filter \"{$name}\" not callable");
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
     * trigger Smarty error
     * 
     * @param string $error_msg 
     * @param integer $error_type 
     */
    public function trigger_error($error_msg, $error_type = E_USER_WARNING)
    {
        throw new SmartyException("Smarty error: $error_msg");
    } 

    /**
     * Return internal filter name
     * 
     * @param callback $function_name 
     */
    public function _get_filter_name($function_name)
    {
        if (is_array($function_name)) {
            $_class_name = (is_object($function_name[0]) ?
                get_class($function_name[0]) : $function_name[0]);
            return $_class_name . '_' . $function_name[1];
        } else {
            return $function_name;
        } 
    } 

    /**
     * Adds directory of plugin files
     * 
     * @param object $smarty 
     * @param string $ |array $ plugins folder
     * @return 
     */
    function addPluginsDir($plugins_dir)
    {
        $this->plugins_dir = array_merge((array)$this->plugins_dir, (array)$plugins_dir);
        $this->plugins_dir = array_unique($this->plugins_dir);
        return;
    } 

    /**
     * Returns a single or all global  variables
     * 
     * @param object $smarty 
     * @param string $varname variable name or null
     * @return string variable value or or array of variables
     */
    function getGlobal($varname = null)
    {
        if (isset($varname)) {
            if (isset($this->global_tpl_vars[$varname])) {
                return $this->global_tpl_vars[$varname]->value;
            } else {
                return '';
            } 
        } else {
            $_result = array();
            foreach ($this->global_tpl_vars AS $key => $var) {
                $_result[$key] = $var->value;
            } 
            return $_result;
        } 
    } 

    /**
     * return a reference to a registered object
     * 
     * @param string $name object name
     * @return object 
     */
    function getRegisteredObject($name)
    {
        if (!isset($this->registered_objects[$name]))
            throw new SmartyException("'$name' is not a registered object");

        if (!is_object($this->registered_objects[$name][0]))
            throw new SmartyException("registered '$name' is not an object");

        return $this->registered_objects[$name][0];
    } 

    /**
     * return name of debugging template
     * 
     * @return string 
     */
    function getDebugTemplate()
    {
        return $this->debug_tpl;
    } 

    /**
     * set the debug template
     * 
     * @param string $tpl_name 
     * @return bool 
     */
    function setDebugTemplate($tpl_name)
    {
        return $this->debug_tpl = $tpl_name;
    } 

    /**
     * lazy loads (valid) property objects
     * 
     * @param string $name property name
     */
    public function __get($name)
    {
        if (in_array($name, array('register', 'unregister', 'utility', 'cache'))) {
            $class = "Smarty_Internal_" . ucfirst($name);
            $this->$name = new $class($this);
            return $this->$name;
        } else if ($name == '_version') {
            // Smarty 2 BC
            $this->_version = self::SMARTY_VERSION;
            return $this->_version;
        } 
        return null;
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
        static $camel_func;
        if (!isset($camel_func))
            $camel_func = create_function('$c', 'return "_" . strtolower($c[1]);'); 
        // PHP4 call to constructor?
        if (strtolower($name) == 'smarty') {
            throw new SmartyException('Please use parent::__construct() to call parent constuctor');
            return false;
        } 
        // see if this is a set/get for a property
        $first3 = strtolower(substr($name, 0, 3));
        if (in_array($first3, array('set', 'get')) && substr($name, 3, 1) !== '_') {
            // try to keep case correct for future PHP 6.0 case-sensitive class methods
            // lcfirst() not available < PHP 5.3.0, so improvise
            $property_name = strtolower(substr($name, 3, 1)) . substr($name, 4); 
            // convert camel case to underscored name
            $property_name = preg_replace_callback('/([A-Z])/', $camel_func, $property_name);
            if (!property_exists($this, $property_name)) {
                throw new SmartyException("property '$property_name' does not exist.");
                return false;
            } 
            if ($first3 == 'get')
                return $this->$property_name;
            else
                return $this->$property_name = $args[0];
        } 
        // Smarty Backward Compatible wrapper
        if (!isset($this->wrapper)) {
            $this->wrapper = new Smarty_Internal_Wrapper($this);
        } 
        return $this->wrapper->convert($name, $args);
    } 
} 

/**
 * Autoloader
 */
function smartyAutoload($class)
{
    $_class = strtolower($class);
    if (substr($_class, 0, 16) === 'smarty_internal_' || $_class == 'smarty_security') {
        include SMARTY_SYSPLUGINS_DIR . $_class . '.php';
    } 
} 

/**
 * Smarty exception class
 */
Class SmartyException extends Exception {
}

/**
 * Smarty compiler exception class
 */
Class SmartyCompilerException extends SmartyException  {
}

?>
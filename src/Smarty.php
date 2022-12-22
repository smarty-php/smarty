<?php

namespace Smarty;

use Smarty\Smarty\Runtime\CaptureRuntime;
use Smarty\Smarty\Runtime\ForeachRuntime;
use Smarty\Smarty\Runtime\InheritanceRuntime;
use Smarty\Smarty\Runtime\MakeNocacheRuntime;
use Smarty\Smarty\Runtime\TplFunctionRuntime;

/**
 * Project:     Smarty: the PHP compiling template engine
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * For questions, help, comments, discussion, etc., please join the
 * Smarty mailing list. Send a blank e-mail to
 * smarty-discussion-subscribe@googlegroups.com
 *
 * @link      https://www.smarty.net/
 * @copyright 2018 New Digital Group, Inc.
 * @copyright 2018 Uwe Tews
 * @author    Monte Ohrt <monte at ohrt dot com>
 * @author    Uwe Tews   <uwe dot tews at gmail dot com>
 * @author    Rodney Rehm
 * @author    Simon Wisselink
 * @package   Smarty
 */

/**
 * This is the main Smarty class
 */
class Smarty extends \Smarty\TemplateBase
{
    /**
     * smarty version
     */
    const SMARTY_VERSION = '4.3.0';
    /**
     * define variable scopes
     */
    const SCOPE_LOCAL    = 1;
    const SCOPE_PARENT   = 2;
    const SCOPE_TPL_ROOT = 4;
    const SCOPE_ROOT     = 8;
    const SCOPE_SMARTY   = 16;
    const SCOPE_GLOBAL   = 32;
    /**
     * define caching modes
     */
    const CACHING_OFF              = 0;
    const CACHING_LIFETIME_CURRENT = 1;
    const CACHING_LIFETIME_SAVED   = 2;
    /**
     * define constant for clearing cache files be saved expiration dates
     */
    const CLEAR_EXPIRED = -1;
    /**
     * define compile check modes
     */
    const COMPILECHECK_OFF       = 0;
    const COMPILECHECK_ON        = 1;
    /**
     * filter types
     */
    const FILTER_POST     = 'post';
    const FILTER_PRE      = 'pre';
    const FILTER_OUTPUT   = 'output';
    const FILTER_VARIABLE = 'variable';
    /**
     * plugin types
     */
    const PLUGIN_FUNCTION         = 'function';
    const PLUGIN_BLOCK            = 'block';
    const PLUGIN_COMPILER         = 'compiler';
    const PLUGIN_MODIFIER         = 'modifier';
    const PLUGIN_MODIFIERCOMPILER = 'modifiercompiler';

    /**
     * assigned global tpl vars
     */
    public static $global_tpl_vars = array();

    /**
     * The character set to adhere to (defaults to "UTF-8")
     */
    public static $_CHARSET = 'UTF-8';

    /**
     * The date format to be used internally
     * (accepts date() and strftime())
     */
    public static $_DATE_FORMAT = '%b %e, %Y';

    /**
     * Flag denoting if PCRE should run in UTF-8 mode
     */
    public static $_UTF8_MODIFIER = 'u';

    /**
     * Flag denoting if operating system is windows
     */
    public static $_IS_WINDOWS = false;

    /**
     * auto literal on delimiters with whitespace
     *
     * @var boolean
     */
    public $auto_literal = true;

    /**
     * display error on not assigned variables
     *
     * @var boolean
     */
    public $error_unassigned = false;

    /**
     * look up relative file path in include_path
     *
     * @var boolean
     */
    public $use_include_path = false;

    /**
     * flag if template_dir is normalized
     *
     * @var bool
     */
    public $_templateDirNormalized = false;

    /**
     * joined template directory string used in cache keys
     *
     * @var string
     */
    public $_joined_template_dir = null;

    /**
     * flag if config_dir is normalized
     *
     * @var bool
     */
    public $_configDirNormalized = false;

    /**
     * joined config directory string used in cache keys
     *
     * @var string
     */
    public $_joined_config_dir = null;

    /**
     * default template handler
     *
     * @var callable
     */
    public $default_template_handler_func = null;

    /**
     * default config handler
     *
     * @var callable
     */
    public $default_config_handler_func = null;

    /**
     * default plugin handler
     *
     * @var callable
     */
    public $default_plugin_handler_func = null;

    /**
     * flag if template_dir is normalized
     *
     * @var bool
     */
    public $_compileDirNormalized = false;

    /**
     * flag if plugins_dir is normalized
     *
     * @var bool
     */
    public $_pluginsDirNormalized = false;

    /**
     * flag if template_dir is normalized
     *
     * @var bool
     */
    public $_cacheDirNormalized = false;

    /**
     * force template compiling?
     *
     * @var boolean
     */
    public $force_compile = false;

    /**
     * use sub dirs for compiled/cached files?
     *
     * @var boolean
     */
    public $use_sub_dirs = false;

    /**
     * allow ambiguous resources (that are made unique by the resource handler)
     *
     * @var boolean
     */
    public $allow_ambiguous_resources = false;

    /**
     * merge compiled includes
     *
     * @var boolean
     */
    public $merge_compiled_includes = false;

    /*
    * flag for behaviour when extends: resource  and {extends} tag are used simultaneous
    *   if false disable execution of {extends} in templates called by extends resource.
    *   (behaviour as versions < 3.1.28)
    *
    * @var boolean
    */
    public $extends_recursion = true;

    /**
     * force cache file creation
     *
     * @var boolean
     */
    public $force_cache = false;

    /**
     * template left-delimiter
     *
     * @var string
     */
    public $left_delimiter = "{";

    /**
     * template right-delimiter
     *
     * @var string
     */
    public $right_delimiter = "}";

    /**
     * array of strings which shall be treated as literal by compiler
     *
     * @var array string
     */
    public $literals = array();

    /**
     * class name
     * This should be instance of \Smarty_Security.
     *
     * @var string
     * @see \Smarty\Security
     */
    public $security_class = \Smarty\Security::class;

    /**
     * implementation of security class
     *
     * @var \Smarty\Security
     */
    public $security_policy = null;

    /**
     * controls if the php template file resource is allowed
     *
     * @var bool
     */
    public $allow_php_templates = false;

    /**
     * debug mode
     * Setting this to true enables the debug-console.
     *
     * @var boolean
     */
    public $debugging = false;

    /**
     * This determines if debugging is enable-able from the browser.
     * <ul>
     *  <li>NONE => no debugging control allowed</li>
     *  <li>URL => enable debugging when SMARTY_DEBUG is found in the URL.</li>
     * </ul>
     *
     * @var string
     */
    public $debugging_ctrl = 'NONE';

    /**
     * Name of debugging URL-param.
     * Only used when $debugging_ctrl is set to 'URL'.
     * The name of the URL-parameter that activates debugging.
     *
     * @var string
     */
    public $smarty_debug_id = 'SMARTY_DEBUG';

    /**
     * Path of debug template.
     *
     * @var string
     */
    public $debug_tpl = null;

    /**
     * When set, smarty uses this value as error_reporting-level.
     *
     * @var int
     */
    public $error_reporting = null;

    /**
     * Controls whether variables with the same name overwrite each other.
     *
     * @var boolean
     */
    public $config_overwrite = true;

    /**
     * Controls whether config values of on/true/yes and off/false/no get converted to boolean.
     *
     * @var boolean
     */
    public $config_booleanize = true;

    /**
     * Controls whether hidden config sections/vars are read from the file.
     *
     * @var boolean
     */
    public $config_read_hidden = false;

    /**
     * locking concurrent compiles
     *
     * @var boolean
     */
    public $compile_locking = true;

    /**
     * Controls whether cache resources should use locking mechanism
     *
     * @var boolean
     */
    public $cache_locking = false;

    /**
     * seconds to wait for acquiring a lock before ignoring the write lock
     *
     * @var float
     */
    public $locking_timeout = 10;

    /**
     * resource type used if none given
     * Must be a valid key of $registered_resources.
     *
     * @var string
     */
    public $default_resource_type = 'file';

    /**
     * caching type
     * Must be an element of $cache_resource_types.
     *
     * @var string
     */
    public $caching_type = 'file';

    /**
     * config type
     *
     * @var string
     */
    public $default_config_type = 'file';

    /**
     * check If-Modified-Since headers
     *
     * @var boolean
     */
    public $cache_modified_check = false;

    /**
     * registered plugins
     *
     * @var array
     */
    public $registered_plugins = array();

    /**
     * registered objects
     *
     * @var array
     */
    public $registered_objects = array();

    /**
     * registered classes
     *
     * @var array
     */
    public $registered_classes = array();

    /**
     * registered filters
     *
     * @var array
     */
    public $registered_filters = array();

    /**
     * registered resources
     *
     * @var array
     */
    public $registered_resources = array();

    /**
     * registered cache resources
     *
     * @var array
     */
    public $registered_cache_resources = array();

    /**
     * default modifier
     *
     * @var array
     */
    public $default_modifiers = array();

    /**
     * autoescape variable output
     *
     * @var boolean
     */
    public $escape_html = false;

    /**
     * start time for execution time calculation
     *
     * @var int
     */
    public $start_time = 0;

    /**
     * required by the compiler for BC
     *
     * @var string
     */
    public $_current_file = null;

    /**
     * internal flag to enable parser debugging
     *
     * @var bool
     */
    public $_parserdebug = false;

    /**
     * This object type (Smarty = 1, template = 2, data = 4)
     *
     * @var int
     */
    public $_objType = 1;

    /**
     * Debug object
     *
     * @var \Smarty\Debug
     */
    public $_debug = null;

    /**
     * template directory
     *
     * @var array
     */
    protected $template_dir = array('./templates/');

    /**
     * flags for normalized template directory entries
     *
     * @var array
     */
    protected $_processedTemplateDir = array();

    /**
     * config directory
     *
     * @var array
     */
    protected $config_dir = array('./configs/');

    /**
     * flags for normalized template directory entries
     *
     * @var array
     */
    protected $_processedConfigDir = array();

    /**
     * compile directory
     *
     * @var string
     */
    protected $compile_dir = './templates_c/';

    /**
     * plugins directory
     *
     * @var array
     */
    protected $plugins_dir = array();

    /**
     * cache directory
     *
     * @var string
     */
    protected $cache_dir = './cache/';

    /**
     * PHP7 Compatibility mode
     * @var bool
     */
    private $isMutingUndefinedOrNullWarnings = false;

	/**
	 * Cache of loaded resource handlers.
	 * @var array
	 */
	public $_resource_handlers = [];

	/**
	 * Cache of loaded cacheresource handlers.
	 * @var array
	 */
	public $_cacheresource_handlers = [];

    /**
     * Initialize new Smarty object
     */
    public function __construct()
    {
        $this->_clearTemplateCache();
        parent::__construct();
        if (is_callable('mb_internal_encoding')) {
            mb_internal_encoding(\Smarty\Smarty::$_CHARSET);
        }
        $this->start_time = microtime(true);
        if (isset($_SERVER[ 'SCRIPT_NAME' ])) {
	        \Smarty\Smarty::$global_tpl_vars[ 'SCRIPT_NAME' ] = new \Smarty\Variable($_SERVER[ 'SCRIPT_NAME' ]);
        }
        // Check if we're running on Windows
        \Smarty\Smarty::$_IS_WINDOWS = strtoupper(substr(PHP_OS, 0, 3)) === 'WIN';
        // let PCRE (preg_*) treat strings as ISO-8859-1 if we're not dealing with UTF-8
        if (\Smarty\Smarty::$_CHARSET !== 'UTF-8') {
            \Smarty\Smarty::$_UTF8_MODIFIER = '';
        }
    }

    /**
     * Check if a template resource exists
     *
     * @param string $resource_name template name
     *
     * @return bool status
     * @throws \Smarty\Exception
     */
    public function templateExists($resource_name)
    {
        // create source object
        $source = Template\Source::load(null, $this, $resource_name);
        return $source->exists;
    }

    /**
     * Loads security class and enables security
     *
     * @param string|\Smarty\Security $security_class if a string is used, it must be class-name
     *
     * @return Smarty                 current Smarty instance for chaining
     * @throws \Smarty\Exception
     */
    public function enableSecurity($security_class = null)
    {
        \Smarty\Security::enableSecurity($this, $security_class);
        return $this;
    }

    /**
     * Disable security
     *
     * @return Smarty current Smarty instance for chaining
     */
    public function disableSecurity()
    {
        $this->security_policy = null;
        return $this;
    }

    /**
     * Add template directory(s)
     *
     * @param string|array $template_dir directory(s) of template sources
     * @param string       $key          of the array element to assign the template dir to
     * @param bool         $isConfig     true for config_dir
     *
     * @return Smarty          current Smarty instance for chaining
     */
    public function addTemplateDir($template_dir, $key = null, $isConfig = false)
    {
        if ($isConfig) {
            $processed = &$this->_processedConfigDir;
            $dir = &$this->config_dir;
            $this->_configDirNormalized = false;
        } else {
            $processed = &$this->_processedTemplateDir;
            $dir = &$this->template_dir;
            $this->_templateDirNormalized = false;
        }
        if (is_array($template_dir)) {
            foreach ($template_dir as $k => $v) {
                if (is_int($k)) {
                    // indexes are not merged but appended
                    $dir[] = $v;
                } else {
                    // string indexes are overridden
                    $dir[ $k ] = $v;
                    unset($processed[ $key ]);
                }
            }
        } else {
            if ($key !== null) {
                // override directory at specified index
                $dir[ $key ] = $template_dir;
                unset($processed[ $key ]);
            } else {
                // append new directory
                $dir[] = $template_dir;
            }
        }
        return $this;
    }

    /**
     * Get template directories
     *
     * @param mixed $index    index of directory to get, null to get all
     * @param bool  $isConfig true for config_dir
     *
     * @return array|string list of template directories, or directory of $index
     */
    public function getTemplateDir($index = null, $isConfig = false)
    {
        if ($isConfig) {
            $dir = &$this->config_dir;
        } else {
            $dir = &$this->template_dir;
        }
        if ($isConfig ? !$this->_configDirNormalized : !$this->_templateDirNormalized) {
            $this->_normalizeTemplateConfig($isConfig);
        }
        if ($index !== null) {
            return isset($dir[ $index ]) ? $dir[ $index ] : null;
        }
        return $dir;
    }

    /**
     * Set template directory
     *
     * @param string|array $template_dir directory(s) of template sources
     * @param bool         $isConfig     true for config_dir
     *
     * @return Smarty current Smarty instance for chaining
     */
    public function setTemplateDir($template_dir, $isConfig = false)
    {
        if ($isConfig) {
            $this->config_dir = array();
            $this->_processedConfigDir = array();
        } else {
            $this->template_dir = array();
            $this->_processedTemplateDir = array();
        }
        $this->addTemplateDir($template_dir, null, $isConfig);
        return $this;
    }

    /**
     * Add config directory(s)
     *
     * @param string|array $config_dir directory(s) of config sources
     * @param mixed        $key        key of the array element to assign the config dir to
     *
     * @return Smarty current Smarty instance for chaining
     */
    public function addConfigDir($config_dir, $key = null)
    {
        return $this->addTemplateDir($config_dir, $key, true);
    }

    /**
     * Get config directory
     *
     * @param mixed $index index of directory to get, null to get all
     *
     * @return array configuration directory
     */
    public function getConfigDir($index = null)
    {
        return $this->getTemplateDir($index, true);
    }

    /**
     * Set config directory
     *
     * @param $config_dir
     *
     * @return Smarty       current Smarty instance for chaining
     */
    public function setConfigDir($config_dir)
    {
        return $this->setTemplateDir($config_dir, true);
    }

    /**
     * Adds directory of plugin files
     *
     * @param null|array|string $plugins_dir
     *
     * @return Smarty current Smarty instance for chaining
     */
    public function addPluginsDir($plugins_dir)
    {
        $this->plugins_dir = array_merge($this->plugins_dir, (array)$plugins_dir);
        $this->_pluginsDirNormalized = false;
        return $this;
    }

    /**
     * Set plugins directory
     *
     * @param string|array $plugins_dir directory(s) of plugins
     *
     * @return Smarty       current Smarty instance for chaining
     */
    public function setPluginsDir($plugins_dir)
    {
        $this->plugins_dir = (array)$plugins_dir;
        $this->_pluginsDirNormalized = false;
        return $this;
    }

    /**
     * Get compiled directory
     *
     * @return string path to compiled templates
     */
    public function getCompileDir()
    {
        if (!$this->_compileDirNormalized) {
            $this->_normalizeDir('compile_dir', $this->compile_dir);
            $this->_compileDirNormalized = true;
        }
        return $this->compile_dir;
    }

    /**
     *
     * @param  string $compile_dir directory to store compiled templates in
     *
     * @return Smarty current Smarty instance for chaining
     */
    public function setCompileDir($compile_dir)
    {
        $this->_normalizeDir('compile_dir', $compile_dir);
        $this->_compileDirNormalized = true;
        return $this;
    }

    /**
     * Get cache directory
     *
     * @return string path of cache directory
     */
    public function getCacheDir()
    {
        if (!$this->_cacheDirNormalized) {
            $this->_normalizeDir('cache_dir', $this->cache_dir);
            $this->_cacheDirNormalized = true;
        }
        return $this->cache_dir;
    }

    /**
     * Set cache directory
     *
     * @param string $cache_dir directory to store cached templates in
     *
     * @return Smarty current Smarty instance for chaining
     */
    public function setCacheDir($cache_dir)
    {
        $this->_normalizeDir('cache_dir', $cache_dir);
        $this->_cacheDirNormalized = true;
        return $this;
    }

    /**
     * creates a template object
     *
     * @param string  $template   the resource handle of the template file
     * @param mixed   $cache_id   cache id to be used with this template
     * @param mixed   $compile_id compile id to be used with this template
     * @param object  $parent     next higher level of Smarty variables
     * @param boolean $do_clone   flag is Smarty object shall be cloned
     *
     * @return \Smarty\Template template object
     * @throws \Smarty\Exception
     */
    public function createTemplate($template, $cache_id = null, $compile_id = null, $parent = null, $do_clone = true)
    {
        if ($cache_id !== null && (is_object($cache_id) || is_array($cache_id))) {
            $parent = $cache_id;
            $cache_id = null;
        }
        if ($parent !== null && is_array($parent)) {
            $data = $parent;
            $parent = null;
        } else {
            $data = null;
        }
        if (!$this->_templateDirNormalized) {
            $this->_normalizeTemplateConfig(false);
        }
        $_templateId = $this->_getTemplateId($template, $cache_id, $compile_id);
        $tpl = null;
        if ($this->caching && isset(\Smarty\Template::$isCacheTplObj[ $_templateId ])) {
            $tpl = $do_clone ? clone \Smarty\Template::$isCacheTplObj[ $_templateId ] :
                \Smarty\Template::$isCacheTplObj[ $_templateId ];
            $tpl->inheritance = null;
            $tpl->tpl_vars = $tpl->config_vars = array();
        } elseif (!$do_clone && isset(\Smarty\Template::$tplObjCache[ $_templateId ])) {
            $tpl = clone \Smarty\Template::$tplObjCache[ $_templateId ];
            $tpl->inheritance = null;
            $tpl->tpl_vars = $tpl->config_vars = array();
        } else {
            /* @var \Smarty\Template $tpl */
            $tpl = new $this->template_class($template, $this, null, $cache_id, $compile_id, null, null);
            $tpl->templateId = $_templateId;
        }
        if ($do_clone) {
            $tpl->smarty = clone $tpl->smarty;
        }
        $tpl->parent = $parent ? $parent : $this;
        // fill data if present
        if (!empty($data) && is_array($data)) {
            // set up variable values
            foreach ($data as $_key => $_val) {
                $tpl->tpl_vars[ $_key ] = new \Smarty\Variable($_val);
            }
        }
        if ($this->debugging || $this->debugging_ctrl === 'URL') {
            $tpl->smarty->_debug = new \Smarty\Debug();
            // check URL debugging control
            if (!$this->debugging && $this->debugging_ctrl === 'URL') {
                $tpl->smarty->_debug->debugUrl($tpl->smarty);
            }
        }
        return $tpl;
    }

    /**
     * Get unique template id
     *
     * @param string                    $template_name
     * @param null|mixed                $cache_id
     * @param null|mixed                $compile_id
     * @param null                      $caching
     * @param \Smarty\Template $template
     *
     * @return string
     * @throws \Smarty\Exception
     */
    public function _getTemplateId(
	    $template_name,
	    $cache_id = null,
	    $compile_id = null,
	    $caching = null,
	    \Smarty\Template $template = null
    ) {
        $template_name = (strpos($template_name, ':') === false) ? "{$this->default_resource_type}:{$template_name}" :
            $template_name;
        $cache_id = $cache_id === null ? $this->cache_id : $cache_id;
        $compile_id = $compile_id === null ? $this->compile_id : $compile_id;
        $caching = (int)($caching === null ? $this->caching : $caching);
        if ((isset($template) && strpos($template_name, ':.') !== false) || $this->allow_ambiguous_resources) {
            $_templateId =
                \Smarty\Resource\BasePlugin::getUniqueTemplateName((isset($template) ? $template : $this), $template_name) .
                "#{$cache_id}#{$compile_id}#{$caching}";
        } else {
            $_templateId = $this->_joined_template_dir . "#{$template_name}#{$cache_id}#{$compile_id}#{$caching}";
        }
        if (isset($_templateId[ 150 ])) {
            $_templateId = sha1($_templateId);
        }
        return $_templateId;
    }

    /**
     * Normalize path
     *  - remove /./ and /../
     *  - make it absolute if required
     *
     * @param string $path     file path
     * @param bool   $realpath if true - convert to absolute
     *                         false - convert to relative
     *                         null - keep as it is but
     *                         remove /./ /../
     *
     * @return string
     */
    public function _realpath($path, $realpath = null)
    {
        $nds = array('/' => '\\', '\\' => '/');
        preg_match(
            '%^(?<root>(?:[[:alpha:]]:[\\\\/]|/|[\\\\]{2}[[:alpha:]]+|[[:print:]]{2,}:[/]{2}|[\\\\])?)(?<path>(.*))$%u',
            $path,
            $parts
        );
        $path = $parts[ 'path' ];
        if ($parts[ 'root' ] === '\\') {
            $parts[ 'root' ] = substr(getcwd(), 0, 2) . $parts[ 'root' ];
        } else {
            if ($realpath !== null && !$parts[ 'root' ]) {
                $path = getcwd() . DIRECTORY_SEPARATOR . $path;
            }
        }
        // normalize DIRECTORY_SEPARATOR
        $path = str_replace($nds[ DIRECTORY_SEPARATOR ], DIRECTORY_SEPARATOR, $path);
        $parts[ 'root' ] = str_replace($nds[ DIRECTORY_SEPARATOR ], DIRECTORY_SEPARATOR, $parts[ 'root' ]);
        do {
            $path = preg_replace(
                array('#[\\\\/]{2}#', '#[\\\\/][.][\\\\/]#', '#[\\\\/]([^\\\\/.]+)[\\\\/][.][.][\\\\/]#'),
                DIRECTORY_SEPARATOR,
                $path,
                -1,
                $count
            );
        } while ($count > 0);
        return $realpath !== false ? $parts[ 'root' ] . $path : str_ireplace(getcwd(), '.', $parts[ 'root' ] . $path);
    }

    /**
     * Empty template objects cache
     */
    public function _clearTemplateCache()
    {
        \Smarty\Template::$isCacheTplObj = array();
        \Smarty\Template::$tplObjCache = array();
    }

    /**
     * @param boolean $use_sub_dirs
     */
    public function setUseSubDirs($use_sub_dirs)
    {
        $this->use_sub_dirs = $use_sub_dirs;
    }

    /**
     * @param int $error_reporting
     */
    public function setErrorReporting($error_reporting)
    {
        $this->error_reporting = $error_reporting;
    }

    /**
     * @param boolean $escape_html
     */
    public function setEscapeHtml($escape_html)
    {
        $this->escape_html = $escape_html;
    }

    /**
     * Return auto_literal flag
     *
     * @return boolean
     */
    public function getAutoLiteral()
    {
        return $this->auto_literal;
    }

    /**
     * Set auto_literal flag
     *
     * @param boolean $auto_literal
     */
    public function setAutoLiteral($auto_literal = true)
    {
        $this->auto_literal = $auto_literal;
    }

    /**
     * @param boolean $force_compile
     */
    public function setForceCompile($force_compile)
    {
        $this->force_compile = $force_compile;
    }

    /**
     * @param boolean $merge_compiled_includes
     */
    public function setMergeCompiledIncludes($merge_compiled_includes)
    {
        $this->merge_compiled_includes = $merge_compiled_includes;
    }

    /**
     * Get left delimiter
     *
     * @return string
     */
    public function getLeftDelimiter()
    {
        return $this->left_delimiter;
    }

    /**
     * Set left delimiter
     *
     * @param string $left_delimiter
     */
    public function setLeftDelimiter($left_delimiter)
    {
        $this->left_delimiter = $left_delimiter;
    }

    /**
     * Get right delimiter
     *
     * @return string $right_delimiter
     */
    public function getRightDelimiter()
    {
        return $this->right_delimiter;
    }

    /**
     * Set right delimiter
     *
     * @param string
     */
    public function setRightDelimiter($right_delimiter)
    {
        $this->right_delimiter = $right_delimiter;
    }

    /**
     * @param boolean $debugging
     */
    public function setDebugging($debugging)
    {
        $this->debugging = $debugging;
    }

    /**
     * @param boolean $config_overwrite
     */
    public function setConfigOverwrite($config_overwrite)
    {
        $this->config_overwrite = $config_overwrite;
    }

    /**
     * @param boolean $config_booleanize
     */
    public function setConfigBooleanize($config_booleanize)
    {
        $this->config_booleanize = $config_booleanize;
    }

    /**
     * @param boolean $config_read_hidden
     */
    public function setConfigReadHidden($config_read_hidden)
    {
        $this->config_read_hidden = $config_read_hidden;
    }

    /**
     * @param boolean $compile_locking
     */
    public function setCompileLocking($compile_locking)
    {
        $this->compile_locking = $compile_locking;
    }

    /**
     * @param string $default_resource_type
     */
    public function setDefaultResourceType($default_resource_type)
    {
        $this->default_resource_type = $default_resource_type;
    }

    /**
     * @param string $caching_type
     */
    public function setCachingType($caching_type)
    {
        $this->caching_type = $caching_type;
    }

    /**
     * Test install
     *
     * @param null $errors
     */
    public function testInstall(&$errors = null)
    {
        \Smarty\TestInstall::testInstall($this, $errors);
    }

    /**
     * Get Smarty object
     *
     * @return Smarty
     */
    public function _getSmartyObj()
    {
        return $this;
    }

    /**
     * Normalize and set directory string
     *
     * @param string $dirName cache_dir or compile_dir
     * @param string $dir     filepath of folder
     */
    private function _normalizeDir($dirName, $dir)
    {
        $this->{$dirName} = $this->_realpath(rtrim($dir ?? '', "/\\") . DIRECTORY_SEPARATOR, true);
    }

    /**
     * Normalize template_dir or config_dir
     *
     * @param bool $isConfig true for config_dir
     */
    private function _normalizeTemplateConfig($isConfig)
    {
        if ($isConfig) {
            $processed = &$this->_processedConfigDir;
            $dir = &$this->config_dir;
        } else {
            $processed = &$this->_processedTemplateDir;
            $dir = &$this->template_dir;
        }
        if (!is_array($dir)) {
            $dir = (array)$dir;
        }
        foreach ($dir as $k => $v) {
            if (!isset($processed[ $k ])) {
                $dir[ $k ] = $v = $this->_realpath(rtrim($v ?? '', "/\\") . DIRECTORY_SEPARATOR, true);
                $processed[ $k ] = true;
            }
        }
        $isConfig ? $this->_configDirNormalized = true : $this->_templateDirNormalized = true;
        $isConfig ? $this->_joined_config_dir = join('#', $this->config_dir) :
            $this->_joined_template_dir = join('#', $this->template_dir);
    }

    /**
     * Mutes errors for "undefined index", "undefined array key" and "trying to read property of null".
     *
     * @void
     */
    public function muteUndefinedOrNullWarnings(): void {
        $this->isMutingUndefinedOrNullWarnings = true;
    }

    /**
     * Indicates if Smarty will mute errors for "undefined index", "undefined array key" and "trying to read property of null".
     * @bool
     */
    public function isMutingUndefinedOrNullWarnings(): bool {
        return $this->isMutingUndefinedOrNullWarnings;
    }

	/**
	 * Empty cache for a specific template
	 *
	 * @param string  $template_name template name
	 * @param string  $cache_id      cache id
	 * @param string  $compile_id    compile id
	 * @param integer $exp_time      expiration time
	 * @param string  $type          resource type
	 *
	 * @return int number of cache files deleted
	 * @throws \Smarty\Exception
	 *@link https://www.smarty.net/docs/en/api.clear.cache.tpl
	 *
	 * @api  Smarty::clearCache()
	 */
	public function clearCache(
		       $template_name,
		       $cache_id = null,
		       $compile_id = null,
		       $exp_time = null,
		       $type = null
	) {
		$this->_clearTemplateCache();
		// load cache resource and call clear
		$_cache_resource = \Smarty\Cacheresource\Base::load($this, $type);
		return $_cache_resource->clear($this, $template_name, $cache_id, $compile_id, $exp_time);
	}

	/**
	 * Empty cache folder
	 *
	 * @api  Smarty::clearAllCache()
	 * @link https://www.smarty.net/docs/en/api.clear.all.cache.tpl
	 *
	 * @param integer $exp_time expiration time
	 * @param string  $type     resource type
	 *
	 * @return int number of cache files deleted
	 */
	public function clearAllCache($exp_time = null, $type = null)
	{
		$this->_clearTemplateCache();
		// load cache resource and call clearAll
		$_cache_resource = \Smarty\Cacheresource\Base::load($this, $type);
		return $_cache_resource->clearAll($this, $exp_time);
	}

	/**
	 * Delete compiled template file
	 *
	 * @param string  $resource_name template name
	 * @param string  $compile_id    compile id
	 * @param integer $exp_time      expiration time
	 *
	 * @return int number of template files deleted
	 * @throws \Smarty\Exception
	 *@link https://www.smarty.net/docs/en/api.clear.compiled.template.tpl
	 *
	 * @api  Smarty::clearCompiledTemplate()
	 */
	public function clearCompiledTemplate($resource_name = null, $compile_id = null, $exp_time = null)
	{
		// clear template objects cache
		$this->_clearTemplateCache();
		$_compile_dir = $this->getCompileDir();
		if ($_compile_dir === '/') { //We should never want to delete this!
			return 0;
		}
		$_compile_id = isset($compile_id) ? preg_replace('![^\w]+!', '_', $compile_id) : null;
		$_dir_sep = $this->use_sub_dirs ? DIRECTORY_SEPARATOR : '^';
		if (isset($resource_name)) {
			$_save_stat = $this->caching;
			$this->caching = \Smarty\Smarty::CACHING_OFF;
			/* @var Template $tpl */
			$tpl = $this->createTemplate($resource_name);
			$this->caching = $_save_stat;
			if (!$tpl->source->handler->uncompiled && !$tpl->source->handler->recompiled && $tpl->source->exists) {
				$_resource_part_1 = basename(str_replace('^', DIRECTORY_SEPARATOR, $tpl->compiled->filepath));
				$_resource_part_1_length = strlen($_resource_part_1);
			} else {
				return 0;
			}
			$_resource_part_2 = str_replace('.php', '.cache.php', $_resource_part_1);
			$_resource_part_2_length = strlen($_resource_part_2);
		}
		$_dir = $_compile_dir;
		if ($this->use_sub_dirs && isset($_compile_id)) {
			$_dir .= $_compile_id . $_dir_sep;
		}
		if (isset($_compile_id)) {
			$_compile_id_part = $_compile_dir . $_compile_id . $_dir_sep;
			$_compile_id_part_length = strlen($_compile_id_part);
		}
		$_count = 0;
		try {
			$_compileDirs = new RecursiveDirectoryIterator($_dir);
			// NOTE: UnexpectedValueException thrown for PHP >= 5.3
		} catch (Exception $e) {
			return 0;
		}
		$_compile = new RecursiveIteratorIterator($_compileDirs, RecursiveIteratorIterator::CHILD_FIRST);
		foreach ($_compile as $_file) {
			if (substr(basename($_file->getPathname()), 0, 1) === '.') {
				continue;
			}
			$_filepath = (string)$_file;
			if ($_file->isDir()) {
				if (!$_compile->isDot()) {
					// delete folder if empty
					@rmdir($_file->getPathname());
				}
			} else {
				// delete only php files
				if (substr($_filepath, -4) !== '.php') {
					continue;
				}
				$unlink = false;
				if ((!isset($_compile_id) ||
						(isset($_filepath[ $_compile_id_part_length ]) &&
							$a = !strncmp($_filepath, $_compile_id_part, $_compile_id_part_length)))
					&& (!isset($resource_name) || (isset($_filepath[ $_resource_part_1_length ])
							&& substr_compare(
								$_filepath,
								$_resource_part_1,
								-$_resource_part_1_length,
								$_resource_part_1_length
							) === 0) || (isset($_filepath[ $_resource_part_2_length ])
							&& substr_compare(
								$_filepath,
								$_resource_part_2,
								-$_resource_part_2_length,
								$_resource_part_2_length
							) === 0))
				) {
					if (isset($exp_time)) {
						if (is_file($_filepath) && time() - filemtime($_filepath) >= $exp_time) {
							$unlink = true;
						}
					} else {
						$unlink = true;
					}
				}
				if ($unlink && is_file($_filepath) && @unlink($_filepath)) {
					$_count++;
					if (function_exists('opcache_invalidate')
						&& (!function_exists('ini_get') || strlen(ini_get('opcache.restrict_api')) < 1)
					) {
						opcache_invalidate($_filepath, true);
					} elseif (function_exists('apc_delete_file')) {
						apc_delete_file($_filepath);
					}
				}
			}
		}
		return $_count;
	}

	/**
	 * Compile all template files
	 *
	 * @api Smarty::compileAllTemplates()
	 *
	 * @param string  $extension     file extension
	 * @param bool    $force_compile force all to recompile
	 * @param int     $time_limit
	 * @param int     $max_errors
	 *
	 * @return integer number of template files recompiled
	 */
	public function compileAllTemplates(
		       $extension = '.tpl',
		       $force_compile = false,
		       $time_limit = 0,
		       $max_errors = null
	) {
		return $this->compileAll($extension, $force_compile, $time_limit, $max_errors);
	}

	/**
	 * Compile all config files
	 *
	 * @api Smarty::compileAllConfig()
	 *
	 * @param string  $extension     file extension
	 * @param bool    $force_compile force all to recompile
	 * @param int     $time_limit
	 * @param int     $max_errors
	 *
	 * @return int number of template files recompiled
	 */
	public function compileAllConfig(
		       $extension = '.conf',
		       $force_compile = false,
		       $time_limit = 0,
		       $max_errors = null
	) {
		return $this->compileAll($extension, $force_compile, $time_limit, $max_errors, true);
	}

	/**
	 * Compile all template or config files
	 *
	 * @param string  $extension     template file name extension
	 * @param bool    $force_compile force all to recompile
	 * @param int     $time_limit    set maximum execution time
	 * @param int     $max_errors    set maximum allowed errors
	 * @param bool    $isConfig      flag true if called for config files
	 *
	 * @return int number of template files compiled
	 */
	protected function compileAll(
		       $extension,
		       $force_compile,
		       $time_limit,
		       $max_errors,
		       $isConfig = false
	) {
		// switch off time limit
		if (function_exists('set_time_limit')) {
			@set_time_limit($time_limit);
		}
		$_count = 0;
		$_error_count = 0;
		$sourceDir = $isConfig ? $this->getConfigDir() : $this->getTemplateDir();
		// loop over array of source directories
		foreach ($sourceDir as $_dir) {
			$_dir_1 = new RecursiveDirectoryIterator(
				$_dir,
				defined('FilesystemIterator::FOLLOW_SYMLINKS') ?
					FilesystemIterator::FOLLOW_SYMLINKS : 0
			);
			$_dir_2 = new RecursiveIteratorIterator($_dir_1);
			foreach ($_dir_2 as $_fileinfo) {
				$_file = $_fileinfo->getFilename();
				if (substr(basename($_fileinfo->getPathname()), 0, 1) === '.' || strpos($_file, '.svn') !== false) {
					continue;
				}
				if (substr_compare($_file, $extension, -strlen($extension)) !== 0) {
					continue;
				}
				if ($_fileinfo->getPath() !== substr($_dir, 0, -1)) {
					$_file = substr($_fileinfo->getPath(), strlen($_dir)) . DIRECTORY_SEPARATOR . $_file;
				}
				echo "\n<br>", $_dir, '---', $_file;
				flush();
				$_start_time = microtime(true);
				$_smarty = clone $this;
				//
				$_smarty->_cache = array();
				$_smarty->force_compile = $force_compile;
				try {
					/* @var Template $_tpl */
					$_tpl = new $this->template_class($_file, $_smarty);
					$_tpl->caching = self::CACHING_OFF;
					$_tpl->source =
						$isConfig ? Smarty_Template_Config::load($_tpl) : Smarty_Template_Source::load($_tpl);
					if ($_tpl->mustCompile()) {
						$_tpl->compileTemplateSource();
						$_count++;
						echo ' compiled in  ', microtime(true) - $_start_time, ' seconds';
						flush();
					} else {
						echo ' is up to date';
						flush();
					}
				} catch (Exception $e) {
					echo "\n<br>        ------>Error: ", $e->getMessage(), "<br><br>\n";
					$_error_count++;
				}
				// free memory
				unset($_tpl);
				$_smarty->_clearTemplateCache();
				if ($max_errors !== null && $_error_count === $max_errors) {
					echo "\n<br><br>too many errors\n";
					exit(1);
				}
			}
		}
		echo "\n<br>";
		return $_count;
	}

	/**
	 * check client side cache
	 *
	 * @param \Smarty\Template\Cached   $cached
	 * @param \Smarty\Template $_template
	 * @param string                    $content
	 *
	 * @throws \Exception
	 * @throws \Smarty\Exception
	 */
	public function cacheModifiedCheck(Template\Cached $cached, \Smarty\Template $_template, $content)
	{
		$_isCached = $_template->isCached() && !$_template->compiled->has_nocache_code;
		$_last_modified_date =
			@substr($_SERVER[ 'HTTP_IF_MODIFIED_SINCE' ], 0, strpos($_SERVER[ 'HTTP_IF_MODIFIED_SINCE' ], 'GMT') + 3);
		if ($_isCached && $cached->timestamp <= strtotime($_last_modified_date)) {
			switch (PHP_SAPI) {
				case 'cgi': // php-cgi < 5.3
				case 'cgi-fcgi': // php-cgi >= 5.3
				case 'fpm-fcgi': // php-fpm >= 5.3.3
					header('Status: 304 Not Modified');
					break;
				case 'cli':
					if (/* ^phpunit */
					!empty($_SERVER[ 'SMARTY_PHPUNIT_DISABLE_HEADERS' ]) /* phpunit$ */
					) {
						$_SERVER[ 'SMARTY_PHPUNIT_HEADERS' ][] = '304 Not Modified';
					}
					break;
				default:
					if (/* ^phpunit */
					!empty($_SERVER[ 'SMARTY_PHPUNIT_DISABLE_HEADERS' ]) /* phpunit$ */
					) {
						$_SERVER[ 'SMARTY_PHPUNIT_HEADERS' ][] = '304 Not Modified';
					} else {
						header($_SERVER[ 'SERVER_PROTOCOL' ] . ' 304 Not Modified');
					}
					break;
			}
		} else {
			switch (PHP_SAPI) {
				case 'cli':
					if (/* ^phpunit */
					!empty($_SERVER[ 'SMARTY_PHPUNIT_DISABLE_HEADERS' ]) /* phpunit$ */
					) {
						$_SERVER[ 'SMARTY_PHPUNIT_HEADERS' ][] =
							'Last-Modified: ' . gmdate('D, d M Y H:i:s', $cached->timestamp) . ' GMT';
					}
					break;
				default:
					header('Last-Modified: ' . gmdate('D, d M Y H:i:s', $cached->timestamp) . ' GMT');
					break;
			}
			echo $content;
		}
	}

	/**
	 * Run filters over content
	 * The filters will be lazy loaded if required
	 * class name format: Smarty_FilterType_FilterName
	 * plugin filename format: filtertype.filtername.php
	 * Smarty2 filter plugins could be used
	 *
	 * @param string                   $type     the type of filter ('pre','post','output') which shall run
	 * @param string                   $content  the content which shall be processed by the filters
	 * @param Template $template template object
	 *
	 * @return string                   the filtered content
	 *@throws Exception
	 */
	public function runFilter($type, $content, Template $template)
	{
		// loop over registered filters of specified type
		if (!empty($this->registered_filters[ $type ])) {
			foreach ($this->registered_filters[ $type ] as $key => $name) {
				$content = call_user_func($this->registered_filters[ $type ][ $key ], $content, $template);
			}
		}
		// return filtered output
		return $content;
	}

	/**
	 * include path cache
	 *
	 * @var string
	 */
	private $_include_path = '';

	/**
	 * include path directory cache
	 *
	 * @var array
	 */
	private $_include_dirs = array();

	/**
	 * include path directory cache
	 *
	 * @var array
	 */
	private $_user_dirs = array();

	/**
	 * stream cache
	 *
	 * @var string[][]
	 */
	private $isFile = array();

	/**
	 * stream cache
	 *
	 * @var string[]
	 */
	private $isPath = array();

	/**
	 * stream cache
	 *
	 * @var int[]
	 */
	private $number = array();

	/**
	 * status cache
	 *
	 * @var bool
	 */
	private $_has_stream_include = null;

	/**
	 * Number for array index
	 *
	 * @var int
	 */
	private $counter = 0;

	/**
	 * Check if include path was updated
	 *
	 * @return bool
	 */
	private function isNewIncludePath()
	{
		$_i_path = get_include_path();
		if ($this->_include_path !== $_i_path) {
			$this->_include_dirs = array();
			$this->_include_path = $_i_path;
			$_dirs = (array)explode(PATH_SEPARATOR, $_i_path);
			foreach ($_dirs as $_path) {
				if (is_dir($_path)) {
					$this->_include_dirs[] = $this->_realpath($_path . DIRECTORY_SEPARATOR, true);
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * return array with include path directories
	 *
	 * @return array
	 */
	public function getIncludePathDirs()
	{
		$this->isNewIncludePath();
		return $this->_include_dirs;
	}

	/**
	 * Return full file path from PHP include_path
	 *
	 * @param string[] $dirs
	 * @param string   $file
	 *
	 * @return bool|string full filepath or false
	 */
	public function getIncludePath($dirs, $file)
	{
		if (!($this->_has_stream_include ?? $this->_has_stream_include = function_exists('stream_resolve_include_path'))
		) {
			$this->isNewIncludePath();
		}
		// try PHP include_path
		foreach ($dirs as $dir) {
			$dir_n = $this->number[$dir] ?? $this->number[$dir] = $this->counter++;
			if (isset($this->isFile[ $dir_n ][ $file ])) {
				if ($this->isFile[ $dir_n ][ $file ]) {
					return $this->isFile[ $dir_n ][ $file ];
				} else {
					continue;
				}
			}
			if (isset($this->_user_dirs[ $dir_n ])) {
				if (false === $this->_user_dirs[ $dir_n ]) {
					continue;
				} else {
					$dir = $this->_user_dirs[ $dir_n ];
				}
			} else {
				if ($dir[ 0 ] === '/' || $dir[ 1 ] === ':') {
					$dir = str_ireplace(getcwd(), '.', $dir);
					if ($dir[ 0 ] === '/' || $dir[ 1 ] === ':') {
						$this->_user_dirs[ $dir_n ] = false;
						continue;
					}
				}
				$dir = substr($dir, 2);
				$this->_user_dirs[ $dir_n ] = $dir;
			}
			if ($this->_has_stream_include) {
				$path = stream_resolve_include_path($dir . ($file ?? ''));
				if ($path) {
					return $this->isFile[ $dir_n ][ $file ] = $path;
				}
			} else {
				foreach ($this->_include_dirs as $key => $_i_path) {
					$path = $this->isPath[$key][$dir_n] ?? $this->isPath[$key][$dir_n] = is_dir($_dir_path = $_i_path . $dir) ? $_dir_path : false;
					if ($path === false) {
						continue;
					}
					if (isset($file)) {
						$_file = $this->isFile[ $dir_n ][ $file ] = (is_file($path . $file)) ? $path . $file : false;
						if ($_file) {
							return $_file;
						}
					} else {
						// no file was given return directory path
						return $path;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Writes file in a safe way to disk
	 *
	 * @param string $_filepath complete filepath
	 * @param string $_contents file content
	 *
	 * @return boolean true
	 *@throws Exception
	 */
	public function writeFile($_filepath, $_contents)
	{
		$_error_reporting = error_reporting();
		error_reporting($_error_reporting & ~E_NOTICE & ~E_WARNING);
		$_dirpath = dirname($_filepath);
		// if subdirs, create dir structure
		if ($_dirpath !== '.') {
			$i = 0;
			// loop if concurrency problem occurs
			// see https://bugs.php.net/bug.php?id=35326
			while (!is_dir($_dirpath)) {
				if (@mkdir($_dirpath, 0777, true)) {
					break;
				}
				clearstatcache();
				if (++$i === 3) {
					error_reporting($_error_reporting);
					throw new Exception("unable to create directory {$_dirpath}");
				}
				sleep(1);
			}
		}
		// write to tmp file, then move to overt file lock race condition
		$_tmp_file = $_dirpath . DIRECTORY_SEPARATOR . str_replace(array('.', ','), '_', uniqid('wrt', true));
		if (!file_put_contents($_tmp_file, $_contents)) {
			error_reporting($_error_reporting);
			throw new Exception("unable to write file {$_tmp_file}");
		}
		/*
		 * Windows' rename() fails if the destination exists,
		 * Linux' rename() properly handles the overwrite.
		 * Simply unlink()ing a file might cause other processes
		 * currently reading that file to fail, but linux' rename()
		 * seems to be smart enough to handle that for us.
		 */
		if (\Smarty\Smarty::$_IS_WINDOWS) {
			// remove original file
			if (is_file($_filepath)) {
				@unlink($_filepath);
			}
			// rename tmp file
			$success = @rename($_tmp_file, $_filepath);
		} else {
			// rename tmp file
			$success = @rename($_tmp_file, $_filepath);
			if (!$success) {
				// remove original file
				if (is_file($_filepath)) {
					@unlink($_filepath);
				}
				// rename tmp file
				$success = @rename($_tmp_file, $_filepath);
			}
		}
		if (!$success) {
			error_reporting($_error_reporting);
			throw new Exception("unable to write file {$_filepath}");
		}
		// set file permissions
		@chmod($_filepath, 0666 & ~umask());
		error_reporting($_error_reporting);
		return true;
	}


	private $runtimes = [];

	/**
	 * Loads and returns a runtime extension or null if not found
	 * @param string $type
	 *
	 * @return object|null
	 */
	public function getRuntime(string $type) {

		if (isset($this->runtimes[$type])) {
			return $this->runtimes[$type];
		}

		// Lazy load runtimes when/if needed
		switch ($type) {
			case 'Capture':
				return $this->runtimes[$type] = new Smarty\Runtime\CaptureRuntime();
			case 'Foreach':
				return $this->runtimes[$type] = new Smarty\Runtime\ForeachRuntime();
			case 'Inheritance':
				return $this->runtimes[$type] = new Smarty\Runtime\InheritanceRuntime();
			case 'MakeNocache':
				return $this->runtimes[$type] = new Smarty\Runtime\MakeNocacheRuntime();
			case 'TplFunction':
				return $this->runtimes[$type] = new Smarty\Runtime\TplFunctionRuntime();
		}

		throw new \Smarty\Exception('Trying to load invalid runtime ' . $type);
	}

	/**
	 * Indicates if a runtime is available.
	 *
	 * @param string $type
	 *
	 * @return bool
	 */
	public function hasRuntime(string $type): bool {
		try {
			$this->getRuntime($type);
			return true;
		} catch (\Smarty\Exception $e) {
			return false;
		}
	}

}

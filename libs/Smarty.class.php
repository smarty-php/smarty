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
 * smarty-general-subscribe@lists.php.net
 *
 * You may contact the authors of Smarty by e-mail at:
 * monte@ispi.net
 * andrei@php.net
 *
 * Or, write to:
 * Monte Ohrt
 * Director of Technology, ispi
 * 237 S. 70th suite 220
 * Lincoln, NE 68510
 *
 * The latest version of Smarty can be obtained from:
 * http://smarty.php.net/
 *
 * @link http://smarty.php.net/
 * @copyright 2001,2002 ispi of Lincoln, Inc.
 * @author Monte Ohrt <monte@ispi.net>
 * @author Andrei Zmievski <andrei@php.net>
 * @package Smarty
 * @version 2.5.0
 */

/* $Id$ */

/**
 * DIR_SEP isn't used anymore, but third party apps might
 */
if(!defined('DIR_SEP')) {
	define('DIR_SEP', DIRECTORY_SEPARATOR);
}

/**
 * set SMARTY_DIR to absolute path to Smarty library files.
 * if not defined, include_path will be used. Sets SMARTY_DIR only if user
 * application has not already defined it.
 */

if (!defined('SMARTY_DIR')) {
    define('SMARTY_DIR', dirname(__FILE__) . DIRECTORY_SEPARATOR);
}

define('SMARTY_PHP_PASSTHRU',   0);
define('SMARTY_PHP_QUOTE',      1);
define('SMARTY_PHP_REMOVE',     2);
define('SMARTY_PHP_ALLOW',      3);

/**
 * @package Smarty
 */
class Smarty
{
    /**#@+
     * Smarty Configuration Section
     */

    /**
     * The name of the directory where templates are located.
     * 
     * @var string
     */
    var $template_dir    =  'templates';

    /**
     * The directory where compiled templates are located.
     * 
     * @var string
     */
    var $compile_dir     =  'templates_c';

    /**
     * The directory where config files are located.
     * 
reques     * @var string
     */
    var $config_dir      =  'configs';

    /**
     * An array of directories searched for plugins.
     * 
     * @var array
     */
    var $plugins_dir     =  array('plugins');

    /**
     * If debugging is enabled, a debug console window will display
     * when the page loads (make sure your browser allows unrequested
     * popup windows)
     * 
     * @var boolean
     */
    var $debugging       =  false;

    /**
     * This is the path to the debug console template. If not set,
     * the default one will be used.
     * 
     * @var string
     */
    var $debug_tpl       =  '';

    /**
     * This determines if debugging is enable-able from the browser.
     * <ul>
     *  <li>NONE => no debugging control allowed</li>
     *  <li>URL => enable debugging when SMARTY_DEBUG is found in the URL.</li>
     * </ul>
     * @link http://www.foo.dom/index.php?SMARTY_DEBUG
     * @var string
     */
    var $debugging_ctrl  =  'NONE';

    /**
     * This tells Smarty whether to check for recompiling or not. Recompiling
     * does not need to happen unless a template or config file is changed. 
     * Typically you enable this during development, and disable for
     * production.
     * 
     * @var boolean
     */
    var $compile_check   =  true;

    /**
     * This forces templates to compile every time. Useful for development
     * or debugging.
     * 
     * @var boolean
     */
    var $force_compile   =  false;

    /**
     * This enables template caching.
     * <ul>
     *  <li>0 = no caching</li>
     *  <li>1 = use class cache_lifetime value</li>
     *  <li>2 = use cache_lifetime in cache file</li>
     * </ul>
     * @var integer
     */
    var $caching         =  0;

    /**
     * The name of the directory for cache files.
     * 
     * @var string
     */
    var $cache_dir       =  'cache';

    /**
     * This is the number of seconds cached content will persist.
     * <ul>
     *  <li>0 = always regenerate cache</li>
     *  <li>-1 = never expires</li>
     * </ul>
     * 
     * @var integer
     */
    var $cache_lifetime  =  3600;

    /**
     * Only used when $caching is enabled. If true, then If-Modified-Since headers
     * are respected with cached content, and appropriate HTTP headers are sent.
     * This way repeated hits to a cached page do not send the entire page to the
     * client every time.
     * 
     * @var boolean
     */
    var $cache_modified_check = false;

    /**
     * This determines how Smarty handles "<?php ... ?>" tags in templates.
     * possible values:
     * <ul>
     *  <li>SMARTY_PHP_PASSTHRU -> print tags as plain text</li>
     *  <li>SMARTY_PHP_QUOTE    -> escape tags as entities</li>
     *  <li>SMARTY_PHP_REMOVE   -> remove php tags</li>
     *  <li>SMARTY_PHP_ALLOW    -> execute php tags</li>
     * </ul>
     *
     * @var integer
     */
    var $php_handling    =  SMARTY_PHP_PASSTHRU;

    /**
     * This enables template security. When enabled, many things are restricted
     * in the templates that normally would go unchecked. This is useful when
     * untrusted parties are editing templates and you want a reasonable level
     * of security. (no direct execution of PHP in templates for example)
     * 
     * @var boolean
     */
    var $security       =   false;

    /**
     * This is the list of template directories that are considered secure. This
     * is used only if {@link $security} is enabled. One directory per array
     * element.  {@link $template_dir} is in this list implicitly.
     * 
     * @var array
     */
    var $secure_dir     =   array();

    /**
     * These are the security settings for Smarty. They are used only when
     * {@link $security} is enabled.
     * 
     * @var array
     */
    var $security_settings  = array(
                                    'PHP_HANDLING'    => false,
                                    'IF_FUNCS'        => array('array', 'list',
                                                               'isset', 'empty',
                                                               'count', 'sizeof',
                                                               'in_array', 'is_array',
															   'true','false'),
                                    'INCLUDE_ANY'     => false,
                                    'PHP_TAGS'        => false,
                                    'MODIFIER_FUNCS'  => array('count'),
                                    'ALLOW_CONSTANTS' => false
                                   );

    /**
     * This is an array of directories where trusted php scripts reside.
     * {@link $security} is disabled during their inclusion/execution.
     *
     * @var array
     */
    var $trusted_dir        = array();

    /**
     * The left delimiter used for the template tags.
     * 
     * @var string
     */
    var $left_delimiter  =  '{';

    /**
     * The right delimiter used for the template tags.
     * 
     * @var string
     */
    var $right_delimiter =  '}';

    /**
     * The order in which request variables are registered, similar to
     * variables_order in php.ini E = Environment, G = GET, P = POST,
     * C = Cookies, S = Server
     * 
     * @var string
     */
    var $request_vars_order    = "EGPCS"; 

    /**
     * Indicates wether $HTTP_*_VARS[] (request_use_auto_globals=false)
     * are uses as request-vars or $_*[]-vars. note: if
     * request_use_auto_globals is true, then $request_vars_order has
     * no effect, but the php-ini-value "gpc_order"
     * 
     * @var boolean
     */
    var $request_use_auto_globals      = false;

    /**
     * Set this if you want different sets of compiled files for the same
     * templates. This is useful for things like different languages.
     * Instead of creating separate sets of templates per language, you
     * set different compile_ids like 'en' and 'de'.
     *
     * @var string
     */
    var $compile_id            = null;

    /**
     * This tells Smarty whether or not to use sub dirs in the cache/ and
     * templates_c/ directories. sub directories better organized, but
     * may not work well with PHP safe mode enabled.
     * 
     * @var boolean
     * 
     */
    var $use_sub_dirs          = true;

    /**
     * This is a list of the modifiers to apply to all template variables.
     * Put each modifier in a separate array element in the order you want
     * them applied. example: <code>array('escape:"htmlall"');</code>
     *
     * @var array
     */
    var $default_modifiers        = array();

    /**
     * This is the resource type to be used when not specified
	 * at the beginning of the resource path. examples:
	 * $smarty->display('file:index.tpl');
	 * $smarty->display('db:index.tpl');
	 * $smarty->display('index.tpl'); // will use default resource type
	 * {include file="file:index.tpl"}
	 * {include file="db:index.tpl"}
	 * {include file="index.tpl"} {* will use default resource type *}
     *
     * @var array
     */
    var $default_resource_type    = 'file';	
	
    /**
     * The function used for cache file handling. If not set, built-in caching is used.
     * 
     * @var null|string function name
     */
    var $cache_handler_func   = null;
     
    /**
     * These are the variables from the globals array that are
     * assigned to all templates automatically. This isn't really
     * necessary any more, you can use the $smarty var to access them
     * directly.
     * 
     * @var array
     */
    var $global_assign   =  array('HTTP_SERVER_VARS' => array('SCRIPT_NAME'));

    /**
     * The value of "undefined". Leave it alone :-)
     * 
     * @var null
     */
    var $undefined       =  null;

    /**
     * This indicates which filters are automatically loaded into Smarty.
     * 
     * @var array array of filter names
     */
    var $autoload_filters = array();

    /**#@+
     * @var boolean
     */     
    /**
     * This tells if config file vars of the same name overwrite each other or not.
     * if disabled, same name variables are accumulated in an array.
     */
    var $config_overwrite = true;

    /**
     * This tells whether or not to automatically booleanize config file variables.
     * If enabled, then the strings "on", "true", and "yes" are treated as boolean
     * true, and "off", "false" and "no" are treated as boolean false.
     */
    var $config_booleanize = true;

    /**
     * This tells whether hidden sections [.foobar] are readable from the
     * tempalates or not. Normally you would never allow this since that is
     * the point behind hidden sections: the application can access them, but
     * the templates cannot.
     */
    var $config_read_hidden = false;

    /**
     * This tells whether or not automatically fix newlines in config files.
     * It basically converts \r (mac) or \r\n (dos) to \n
     */
    var $config_fix_newlines = true;
    /**#@-*/
     
    /**
     * If a template cannot be found, this PHP function will be executed.
     * Useful for creating templates on-the-fly or other special action.
     * 
     * @var string function name
     */
    var $default_template_handler_func = '';

    /**
     * The file that contains the compiler class. This can a full
     * pathname, or relative to the php_include path.
     * 
     * @var string
     */
    var $compiler_file        =    'Smarty_Compiler.class.php';

    /**
     * The class used for compiling templates.
     * 
     * @var string
     */
    var $compiler_class        =   'Smarty_Compiler';

    /**
     * The class used to load config vars.
     *
     * @var string
     */
    var $config_class          =   'Config_File';

/**#@+
 * END Smarty Configuration Section
 * There should be no need to touch anything below this line.
 * @access private
 */
    /**
     * error messages. true/false
     *
     * @var boolean
     */
    var $_error_msg            = false;

    /**
     * where assigned template vars are kept
     *
     * @var array
     */
    var $_tpl_vars             = array();

    /**
     * stores run-time $smarty.* vars
     *
     * @var null|array
     */
    var $_smarty_vars          = null;

    /**
     * keeps track of sections
     *
     * @var array
     */
    var $_sections             = array();

    /**
     * keeps track of foreach blocks
     *
     * @var array
     */
    var $_foreach              = array();

    /**
     * keeps track of tag hierarchy
     *
     * @var array
     */
    var $_tag_stack            = array();

    /**
     * configuration object
     *
     * @var Config_file
     */
    var $_conf_obj             = null;

    /**
     * loaded configuration settings
     *
     * @var array
     */
    var $_config               = array(array('vars'  => array(), 'files' => array()));

    /**
     * md5 checksum of the string 'Smarty'
     *
     * @var string
     */
    var $_smarty_md5           = 'f8d698aea36fcbead2b9d5359ffca76f';

    /**
     * Smarty version number
     *
     * @var string
     */
    var $_version              = '2.5.0';

    /**
     * current template inclusion depth
     *
     * @var integer
     */
    var $_inclusion_depth      = 0;

    /**
     * for different compiled templates
     *
     * @var string
     */
    var $_compile_id           = null;

    /**
     * text in URL to enable debug mode
     *
     * @var string
     */
    var $_smarty_debug_id      = 'SMARTY_DEBUG';

    /**
     * debugging information for debug console
     *
     * @var array
     */
    var $_smarty_debug_info    = array();

    /**
     * info that makes up a cache file
     *
     * @var array
     */
    var $_cache_info           = array();

    /**
     * default file permissions
     *
     * @var integer
     */
    var $_file_perms           = 0644;

    /**
     * default dir permissions
     *
     * @var integer
     */
    var $_dir_perms               = 0771;

    /**
     * registered objects
     *
     * @var array
     */
    var $_reg_objects           = array();

    /**
     * table keeping track of plugins
     *
     * @var array
     */
    var $_plugins              = array(
                                       'modifier'      => array(),
                                       'function'      => array(),
                                       'block'         => array(),
                                       'compiler'      => array(),
                                       'prefilter'     => array(),
                                       'postfilter'    => array(),
                                       'outputfilter'  => array(),
                                       'resource'      => array(),
                                       'insert'        => array());

    /**#@-*/
    /**
     * The class constructor.
     *
     * @uses $global_assign uses {@link assign()} to assign each corresponding
     *                      value from $GLOBALS to the template vars
     */
    function Smarty()
    {
        foreach ($this->global_assign as $key => $var_name) {
            if (is_array($var_name)) {
                foreach ($var_name as $var) {
                    if (isset($GLOBALS[$key][$var])) {
                        $this->assign($var, $GLOBALS[$key][$var]);
                    } else {
                        $this->assign($var, $this->undefined);
                    }
                }
            } else {
                if (isset($GLOBALS[$var_name])) {
                    $this->assign($var_name, $GLOBALS[$var_name]);
                } else {
                    $this->assign($var_name, $this->undefined);
                }
            }
        }
    }


    /**
     * assigns values to template variables
     *
     * @param array|string $tpl_var the template variable name(s)
     * @param mixed $value the value to assign
     */
    function assign($tpl_var, $value = null)
    {
        if (is_array($tpl_var)){
            foreach ($tpl_var as $key => $val) {
                if ($key != '') {
                    $this->_tpl_vars[$key] = $val;
                }
            }
        } else {
            if ($tpl_var != '')
                $this->_tpl_vars[$tpl_var] = $value;
        }
    }

    /**
     * assigns values to template variables by reference
     *
     * @param string $tpl_var the template variable name
     * @param mixed $value the referenced value to assign
     */    
    function assign_by_ref($tpl_var, &$value)
    {
        if ($tpl_var != '')
            $this->_tpl_vars[$tpl_var] = &$value;
    }
    
    /**
     * appends values to template variables
     *
     * @param array|string $tpl_var the template variable name(s)
     * @param mixed $value the value to append
     */    
    function append($tpl_var, $value=null, $merge=false)
    {
        if (is_array($tpl_var)) {
			// $tpl_var is an array, ignore $value
            foreach ($tpl_var as $_key => $_val) {
                if ($_key != '') {
					if(!@is_array($this->_tpl_vars[$_key])) {
						settype($this->_tpl_vars[$_key],'array');
					}
					if($merge && is_array($_val)) {
						foreach($_val as $_mkey => $_mval) {
							$this->_tpl_vars[$_key][$_mkey] = $_mval;
						}
					} else {
						$this->_tpl_vars[$_key][] = $_val;
					}
                }
            }
        } else {
            if ($tpl_var != '' && isset($value)) {
				if(!@is_array($this->_tpl_vars[$tpl_var])) {
					settype($this->_tpl_vars[$tpl_var],'array');
				}
				if($merge && is_array($value)) {
					foreach($value as $_mkey => $_mval) {
						$this->_tpl_vars[$tpl_var][$_mkey] = $_mval;
					}
				} else {
					$this->_tpl_vars[$tpl_var][] = $value;
				}
            }
        }
    }

    /**
     * appends values to template variables by reference
     *
     * @param string $tpl_var the template variable name
     * @param mixed $value the referenced value to append
     */    
    function append_by_ref($tpl_var, &$value, $merge=false)
    {
        if ($tpl_var != '' && isset($value)) {
			if(!@is_array($this->_tpl_vars[$tpl_var])) {
			 settype($this->_tpl_vars[$tpl_var],'array');
			}
			if ($merge && is_array($value)) {
				foreach($value as $_key => $_val) {
					$this->_tpl_vars[$tpl_var][$_key] = &$value[$_key];
				}
			} else {
				$this->_tpl_vars[$tpl_var][] = &$value;
			}
        }
    }


    /**
     * clear the given assigned template variable.
     *
     * @param string $tpl_var the template variable to clear
     */    
    function clear_assign($tpl_var)
    {
        if (is_array($tpl_var))
            foreach ($tpl_var as $curr_var)
                unset($this->_tpl_vars[$curr_var]);
        else
            unset($this->_tpl_vars[$tpl_var]);
    }


    /**
     * Registers custom function to be used in templates
     *
     * @param string $function the name of the template function
     * @param string $function_impl the name of the PHP function to register
     */    
    function register_function($function, $function_impl)
    {
        $this->_plugins['function'][$function] =
            array($function_impl, null, null, false);
    }

    /**
     * Unregisters custom function
     *
     * @param string $function name of template function
     */    
    function unregister_function($function)
    {
        unset($this->_plugins['function'][$function]);
    }

    /**
     * Registers object to be used in templates
     *
     * @param string $object name of template object
     * @param object &$object_impl the referenced PHP object to register
     * @param null|array $allowed list of allowed methods (empty = all)
     * @param boolean $smarty_args smarty argument format, else traditional
     * @param null|array $block_functs list of methods that are block format
     */    
    function register_object($object, &$object_impl, $allowed = array(), $smarty_args = true, $block_methods = array())
    {
        settype($allowed, 'array');        
        settype($smarty_args, 'boolean');        
        $this->_reg_objects[$object] =
            array(&$object_impl, $allowed, $smarty_args, $block_methods);
    }

    /**
     * Unregisters object
     *
     * @param string $object name of template object
     */    
    function unregister_object($object)
    {
        unset($this->_reg_objects[$object]);
    }
    
    
    /**
     * Registers block function to be used in templates
     *
     * @param string $block name of template block
     * @param string $block_impl PHP function to register
     */    
    function register_block($block, $block_impl)
    {
        $this->_plugins['block'][$block] =
            array($block_impl, null, null, false);
    }

    /**
     * Unregisters block function
     *
     * @param string $block name of template function
     */    
    function unregister_block($block)
    {
        unset($this->_plugins['block'][$block]);
    }

    /**
     * Registers compiler function
     *
     * @param string $function name of template function
     * @param string $function_impl name of PHP function to register
     */    
    function register_compiler_function($function, $function_impl)
    {
        $this->_plugins['compiler'][$function] =
            array($function_impl, null, null, false);
    }

    /**
     * Unregisters compiler function
     *
     * @param string $function name of template function
     */    
    function unregister_compiler_function($function)
    {
        unset($this->_plugins['compiler'][$function]);
    }

    /**
     * Registers modifier to be used in templates
     *
     * @param string $modifier name of template modifier
     * @param string $modifier_impl name of PHP function to register
     */    
    function register_modifier($modifier, $modifier_impl)
    {
        $this->_plugins['modifier'][$modifier] =
            array($modifier_impl, null, null, false);
    }

    /**
     * Unregisters modifier
     *
     * @param string $modifier name of template modifier
     */    
    function unregister_modifier($modifier)
    {
        unset($this->_plugins['modifier'][$modifier]);
    }

    /**
     * Registers a resource to fetch a template
     *
     * @param string $type name of resource
     * @param array $functions array of functions to handle resource
     */    
    function register_resource($type, $functions)
    {
        if (sizeof($functions)<5) {
            $this->_plugins['resource'][$type] =
                array($functions, false);

        } elseif (sizeof($functions)==5 && is_object($functions[0])) {
            $this->_plugins['resource'][$type] =
                array(array(array(&$functions[0], $functions[1])
                            ,array(&$functions[0], $functions[2])
                            ,array(&$functions[0], $functions[3])
                            ,array(&$functions[0], $functions[4]))
                      ,false);

        } else {
            $this->trigger_error("malformed function-list for '$type' in register_resource");

        }
    }

    /**
     * Unregisters a resource
     *
     * @param string $type name of resource
     */    
    function unregister_resource($type)
    {
        unset($this->_plugins['resource'][$type]);
    }

    /**
     * Registers a prefilter function to apply
     * to a template before compiling
     *
     * @param string $function name of PHP function to register
     */    
    function register_prefilter($function)
    {
	$_name = (is_array($function)) ? $function[1] : $function;
        $this->_plugins['prefilter'][$_name]
            = array($function, null, null, false);
    }

    /**
     * Unregisters a prefilter function
     *
     * @param string $function name of PHP function
     */    
    function unregister_prefilter($function)
    {
        unset($this->_plugins['prefilter'][$function]);
    }

    /**
     * Registers a postfilter function to apply
     * to a compiled template after compilation
     *
     * @param string $function name of PHP function to register
     */    
    function register_postfilter($function)
    {
	$_name = (is_array($function)) ? $function[1] : $function;
        $this->_plugins['postfilter'][$_name]
            = array($function, null, null, false);
    }

    /**
     * Unregisters a postfilter function
     *
     * @param string $function name of PHP function
     */    
    function unregister_postfilter($function)
    {
        unset($this->_plugins['postfilter'][$function]);
    }

    /**
     * Registers an output filter function to apply
     * to a template output
     *
     * @param string $function name of PHP function
     */    
    function register_outputfilter($function)
    {
	$_name = (is_array($function)) ? $function[1] : $function;
        $this->_plugins['outputfilter'][$_name]
            = array($function, null, null, false);
    }

    /**
     * Unregisters an outputfilter function
     *
     * @param string $function name of PHP function
     */    
    function unregister_outputfilter($function)
    {
        unset($this->_plugins['outputfilter'][$function]);
    }    
    
    /**
     * load a filter of specified type and name
     *
     * @param string $type filter type
     * @param string $name filter name
     */    
    function load_filter($type, $name)
    {
        switch ($type) {
            case 'output':
				$_params = array('plugins' => array(array($type . 'filter', $name, null, null, false)));
                $this->_execute_core_function('load_plugins', $_params);
                break;

            case 'pre':
            case 'post':
                if (!isset($this->_plugins[$type . 'filter'][$name]))
                    $this->_plugins[$type . 'filter'][$name] = false;
                break;
        }
    }

    /**
     * clear cached content for the given template and cache id
     *
     * @param string $tpl_file name of template file
     * @param string $cache_id name of cache_id
     * @param string $compile_id name of compile_id
     * @param string $exp_time expiration time
     * @return boolean
     */    
    function clear_cache($tpl_file = null, $cache_id = null, $compile_id = null, $exp_time = null)
    {
        
        if (!isset($compile_id))
            $compile_id = $this->compile_id;

	if (!isset($tpl_file))
	    $compile_id = null;

	$_auto_id = $this->_get_auto_id($cache_id, $compile_id);

        if (!empty($this->cache_handler_func)) {
            return call_user_func_array($this->cache_handler_func,
                                  array('clear', &$this, &$dummy, $tpl_file, $cache_id, $compile_id));
        } else {
			$_params = array('auto_base' => $this->cache_dir,
							'auto_source' => $tpl_file,
							'auto_id' => $_auto_id,
							'exp_time' => $exp_time);
			return $this->_execute_core_function('rm_auto', $_params);
        }
        
    }


    /**
     * clear the entire contents of cache (all templates)
     *
     * @param string $exp_time expire time
     * @return boolean results of {@link smarty_core_rm_auto()}
     */    
    function clear_all_cache($exp_time = null)
    {
        if (!empty($this->cache_handler_func)) {
            call_user_func_array($this->cache_handler_func,
                           array('clear', &$this, &$dummy));
        } else {
			$_params = array('auto_base' => $this->cache_dir,
							'auto_source' => null,
							'auto_id' => null,
							'exp_time' => $exp_time);
			return $this->_execute_core_function('rm_auto', $_params);			
        }
    }


    /**
     * test to see if valid cache exists for this template
     *
     * @param string $tpl_file name of template file
     * @param string $cache_id
     * @param string $compile_id
     * @return string|false results of {@link _read_cache_file()}
     */    
    function is_cached($tpl_file, $cache_id = null, $compile_id = null)
    {
        if (!$this->caching)
            return false;

        if (!isset($compile_id))
            $compile_id = $this->compile_id;

		$_params = array(
			'tpl_file' => $tpl_file,
			'cache_id' => $cache_id,
			'compile_id' => $compile_id
		);
        return $this->_execute_core_function('read_cache_file', $_params);
    }


    /**
     * clear all the assigned template variables.
     *
     */    
    function clear_all_assign()
    {
        $this->_tpl_vars = array();
    }

    /**
     * clears compiled version of specified template resource,
     * or all compiled template files if one is not specified.
     * This function is for advanced use only, not normally needed.
     *
     * @param string $tpl_file
     * @param string $compile_id
     * @param string $exp_time
     * @return boolean results of {@link smarty_core_rm_auto()}
     */    
    function clear_compiled_tpl($tpl_file = null, $compile_id = null, $exp_time = null)
    {
        if (!isset($compile_id)) {
        	$compile_id = $this->compile_id;
		}
		$_params = array('auto_base' => $this->compile_dir,
						'auto_source' => $tpl_file,
						'auto_id' => $compile_id,
						'exp_time' => $exp_time);
		return $this->_execute_core_function('rm_auto', $_params);		
    }

    /**
     * Checks whether requested template exists.
     *
     * @param string $tpl_file
     * @return boolean
     */    
    function template_exists($tpl_file)
    {
		$_params = array('tpl_path' => $tpl_file);
         return $this->_execute_core_function('fetch_template_info', $_params);
    }

    /**
     * Returns an array containing template variables
     *
     * @param string $name
     * @param string $type
     * @return array
     */    
    function &get_template_vars($name=null)
    {
		if(!isset($name)) {
        	return $this->_tpl_vars;
		}
		if(isset($this->_tpl_vars[$name])) {
			return $this->_tpl_vars[$name];
		}
    }

    /**
     * Returns an array containing config variables
     *
     * @param string $name
     * @param string $type
     * @return array
     */    
    function &get_config_vars($name=null)
    {
		if(!isset($name) && is_array($this->_config[0])) {
        	return $this->_config[0]['vars'];
		} else if(isset($this->_config[0]['vars'][$name])) {
			return $this->_config[0]['vars'][$name];
		}
    }

    /**
     * trigger Smarty error
     *
     * @param string $error_msg
     * @param integer $error_type
     */    
    function trigger_error($error_msg, $error_type = E_USER_WARNING)
    {
        trigger_error("Smarty error: $error_msg", $error_type);
    }


    /**
     * executes & displays the template results
     *
     * @param string $tpl_file
     * @param string $cache_id
     * @param string $compile_id
     */    
    function display($tpl_file, $cache_id = null, $compile_id = null)
    {
        $this->fetch($tpl_file, $cache_id, $compile_id, true);
    }

    /**
     * executes & returns or displays the template results
     *
     * @param string $tpl_file
     * @param string $cache_id
     * @param string $compile_id
     * @param boolean $display
     */
    function fetch($tpl_file, $cache_id = null, $compile_id = null, $display = false)
    {
        static $_cache_info = array();

        $_smarty_old_error_level = $this->debugging ? error_reporting() : error_reporting(error_reporting() & ~E_NOTICE);

        if($this->security && !in_array($this->template_dir, $this->secure_dir)) {
            // add template_dir to secure_dir array
            array_unshift($this->secure_dir, $this->template_dir);
        }

        if (!$this->debugging && $this->debugging_ctrl == 'URL'
               && strstr($GLOBALS['HTTP_SERVER_VARS']['QUERY_STRING'], $this->_smarty_debug_id)) {
            // enable debugging from URL
            $this->debugging = true;
        }
		
        if ($this->debugging) {
            // capture time for debugging info
			$_params = array();
            $_debug_start_time = $this->_execute_core_function('get_microtime', $_params);
            $this->_smarty_debug_info[] = array('type'      => 'template',
                                                'filename'  => $tpl_file,
                                                'depth'     => 0);
            $_included_tpls_idx = count($this->_smarty_debug_info) - 1;
        }

        if (!isset($compile_id)) {
            $compile_id = $this->compile_id;
        }

        $this->_compile_id = $compile_id;
        $this->_inclusion_depth = 0;

        if ($this->caching) {
            // save old cache_info, initialize cache_info
            array_push($_cache_info, $this->_cache_info);
            $this->_cache_info = array();
			$_params = array(
				'tpl_file' => $tpl_file,
				'cache_id' => $cache_id,
				'compile_id' => $compile_id,
				'results' => null
			);
            if ($this->_execute_core_function('read_cache_file', $_params)) {
				$_smarty_results = $_params['results'];
                if (@count($this->_cache_info['insert_tags'])) {
					$_params = array('plugins' => $this->_cache_info['insert_tags']);
                	$this->_execute_core_function('load_plugins', $_params);
					$_params = array('results' => $_smarty_results);
                    $_smarty_results = $this->_execute_core_function('process_cached_inserts', $_params);
                }
                if ($display) {
                    if ($this->debugging)
                    {
                        // capture time for debugging info
						$_params = array();
                        $this->_smarty_debug_info[$_included_tpls_idx]['exec_time'] = $this->_execute_core_function('get_microtime', $_params) - $_debug_start_time;
                        $_smarty_results .= $this->_execute_core_function('display_debug_console', $_params);
                    }
                    if ($this->cache_modified_check) {
                        $_last_modified_date = substr($GLOBALS['HTTP_SERVER_VARS']['HTTP_IF_MODIFIED_SINCE'], 0, strpos($GLOBALS['HTTP_SERVER_VARS']['HTTP_IF_MODIFIED_SINCE'], 'GMT') + 3);
                        $_gmt_mtime = gmdate('D, d M Y H:i:s', $this->_cache_info['timestamp']).' GMT';
                        if (@count($this->_cache_info['insert_tags']) == 0
                            && $_gmt_mtime == $_last_modified_date) {
                            header("HTTP/1.1 304 Not Modified");
                        } else {
                            header("Last-Modified: ".$_gmt_mtime);
                            echo $_smarty_results;
                        }
                    } else {
                            echo $_smarty_results;                        
                    }
                    error_reporting($_smarty_old_error_level);
                    // restore initial cache_info
                    $this->_cache_info = array_pop($_cache_info);
                    return true;    
                } else {
                    error_reporting($_smarty_old_error_level);
                    // restore initial cache_info
                    $this->_cache_info = array_pop($_cache_info);
                    return $_smarty_results;
                }
            } else {
                $this->_cache_info['template'][$tpl_file] = true;
                if ($this->cache_modified_check) {
                    header("Last-Modified: ".gmdate('D, d M Y H:i:s', time()).' GMT');
                }
            }
        }		
		
		// load filters that are marked as autoload
        if (count($this->autoload_filters)) {
        	foreach ($this->autoload_filters as $_filter_type => $_filters) {
            	foreach ($_filters as $_filter) {
                	$this->load_filter($_filter_type, $_filter);
            	}
        	}
        }

        $_smarty_compile_path = $this->_get_compile_path($tpl_file);

        // if we just need to display the results, don't perform output
        // buffering - for speed
        if ($display && !$this->caching && count($this->_plugins['outputfilter']) == 0) {
            if ($this->_process_template($tpl_file, $_smarty_compile_path))
            {
                include($_smarty_compile_path);
            }
        } else {
            ob_start();
            if ($this->_process_template($tpl_file, $_smarty_compile_path))
            {
                include($_smarty_compile_path);
            }
            $_smarty_results = ob_get_contents();
            ob_end_clean();

            foreach ((array)$this->_plugins['outputfilter'] as $_output_filter) {
                $_smarty_results = call_user_func_array($_output_filter[0], array($_smarty_results, &$this));
            }
        }

        if ($this->caching) {
			$_params = array('tpl_file' => $tpl_file,
						'cache_id' => $cache_id,
						'compile_id' => $compile_id,
						'results' => $_smarty_results); 
			$this->_execute_core_function('write_cache_file', $_params);
			$_smarty_results = $this->_execute_core_function('process_cached_inserts', $_params);
            // restore initial cache_info
            $this->_cache_info = array_pop($_cache_info);
        }

        if ($display) {
            if (isset($_smarty_results)) { echo $_smarty_results; }
            if ($this->debugging) {
                // capture time for debugging info
				$_params = array();
                $this->_smarty_debug_info[$_included_tpls_idx]['exec_time'] = ($this->_execute_core_function('get_microtime', $_params) - $_debug_start_time);
                echo $this->_execute_core_function('display_debug_console', $_params);
            }
            error_reporting($_smarty_old_error_level);
            return;
        } else {
            error_reporting($_smarty_old_error_level);
            if (isset($_smarty_results)) { return $_smarty_results; }
        }
    }

    /**
     * load configuration values
     *
     * @param string $file
     * @param string $section
     * @param string $scope
     */    
    function config_load($file, $section = null, $scope = 'global')
    {
		require_once($this->_get_plugin_filepath('function', 'config_load'));    
		smarty_function_config_load(array('file' => $file, 'section' => $section, 'scope' => $scope), $this);
    }

    /**
     * return a reference to a registered object
     *
     * @param string $name
     * @return object
     */    
	function &get_registered_object($name) {
		if (!isset($this->_reg_objects[$name]))
		$this->_trigger_fatal_error("'$name' is not a registered object");

		if (!is_object($this->_reg_objects[$name][0]))
		$this->_trigger_fatal_error("registered '$name' is not an object");

		return $this->_reg_objects[$name][0];		
	}	

    /**
     * clear configuration values
     *
     * @param string $var
     */    
    function clear_config($var = null)
    {
        if(!isset($var)) {
            // clear all values
            $this->_config = array(array('vars'  => array(),
                                         'files' => array()));
        } else {
            unset($this->_config[0]['vars'][$var]);            
        }
    }	

    /**
     * Quote subpattern references
     *
     * @param string $string
     * @return string
     */
    function quote_replace($string)
    {
        return preg_replace('![\\$]\d!', '\\\\\\0', $string);
    }
		
    /**
     * execute core function
     * @return mixed value of function return
     */
    function _execute_core_function($funcname, &$params)
    {
		require_once($this->_get_plugin_filepath('core', $funcname));  
		$_core_funcname = 'smarty_core_' . $funcname;
		return $_core_funcname($params, $this);
    }

    /**
     * get filepath of requested plugin
     *
     * @param string $type
     * @param string $name
     * @return string|false
     */    
    function _get_plugin_filepath($type, $name)
    {
        $_plugin_filename = "$type.$name.php";
        
        foreach ((array)$this->plugins_dir as $_plugin_dir) {

            $_plugin_filepath = $_plugin_dir . DIRECTORY_SEPARATOR . $_plugin_filename;

            // see if path is relative
            if (!preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $_plugin_dir)) {
                $_relative_paths[] = $_plugin_dir;
                // relative path, see if it is in the SMARTY_DIR
                if (@is_readable(SMARTY_DIR . $_plugin_filepath)) {
                    return SMARTY_DIR . $_plugin_filepath;
                }
            }
            // try relative to cwd (or absolute)
            if (@is_readable($_plugin_filepath)) {
                return $_plugin_filepath;
            }
        }

        // still not found, try PHP include_path
        if(isset($_relative_paths)) {
            foreach ((array)$_relative_paths as $_plugin_dir) {

                $_plugin_filepath = $_plugin_dir . DIRECTORY_SEPARATOR . $_plugin_filename;

				$_params = array('file_path' => $_plugin_filepath);
            	if($this->_execute_core_function('get_include_path', $_params)) {				
					return $_params['new_file_path'];
                }
            }
        }   
        return false;
    }	
	
   /**
     * umm... process the template
     *
     * @param string $tpl_file
     * @param string $compile_path
     * @return boolean
     */    
    function _process_template($tpl_file, $compile_path)
    {
        // test if template needs to be compiled
        if (!$this->force_compile && file_exists($compile_path)) {
            if (!$this->compile_check) {
                // no need to check if the template needs recompiled
                return true;
            } else {
                // get template source and timestamp
				$_params = array('tpl_path' => $tpl_file);
                if (!$this->_execute_core_function('fetch_template_info', $_params)) {
                    return false;
                }
				$_template_source = $_params['template_source'];
				$_template_timestamp = $_params['template_timestamp'];
                if ($_template_timestamp <= filemtime($compile_path)) {
                    // template not expired, no recompile
                    return true;
                } else {
                    // compile template
                    $this->_compile_template($tpl_file, $_template_source, $_template_compiled);
					$_params = array('compile_path' => $compile_path, 'template_compiled' => $_template_compiled, 'template_timestamp' => $_template_timestamp);
					$this->_execute_core_function('write_compiled_template', $_params);
                    return true;
                }
            }
        } else {
            // compiled template does not exist, or forced compile
			$_params = array('tpl_path' => $tpl_file);
            if (!$this->_execute_core_function('fetch_template_info', $_params)) {
                return false;
            }
			$_template_source = $_params['template_source'];
			$_template_timestamp = $_params['template_timestamp'];
            $this->_compile_template($tpl_file, $_template_source, $_template_compiled);
			$_params = array('compile_path' => $compile_path, 'template_compiled' => $_template_compiled, 'template_timestamp' => $_template_timestamp);
			$this->_execute_core_function('write_compiled_template', $_params);
            return true;
        }
    }

    /**
     * Get the compile path for this template file
     *
     * @param string $tpl_file
     * @return string results of {@link _get_auto_filename()}
     */    
    function _get_compile_path($tpl_file)
    {
        return $this->_get_auto_filename($this->compile_dir, $tpl_file,
                                         $this->_compile_id);
    }

    /**
     * called to compile the templates
     *
     * sets $template_compiled to the compiled template
     * @param string $tpl_file
     * @param string $template_source
     * @param string $template_compiled
     * @return boolean
     */    
    function _compile_template($tpl_file, $template_source, &$template_compiled)
    {		
        if(file_exists(SMARTY_DIR.$this->compiler_file)) {
            require_once SMARTY_DIR.$this->compiler_file;            
        } else {
            // use include_path
            require_once $this->compiler_file;
        }

        $smarty_compiler = new $this->compiler_class;

        $smarty_compiler->template_dir      = $this->template_dir;
        $smarty_compiler->compile_dir       = $this->compile_dir;
        $smarty_compiler->plugins_dir       = $this->plugins_dir;
        $smarty_compiler->config_dir        = $this->config_dir;
        $smarty_compiler->force_compile     = $this->force_compile;
        $smarty_compiler->caching           = $this->caching;
        $smarty_compiler->php_handling      = $this->php_handling;
        $smarty_compiler->left_delimiter    = $this->left_delimiter;
        $smarty_compiler->right_delimiter   = $this->right_delimiter;
        $smarty_compiler->_version          = $this->_version;
        $smarty_compiler->security          = $this->security;
        $smarty_compiler->secure_dir        = $this->secure_dir;
        $smarty_compiler->security_settings = $this->security_settings;
        $smarty_compiler->trusted_dir       = $this->trusted_dir;
        $smarty_compiler->_reg_objects      = &$this->_reg_objects;
        $smarty_compiler->_plugins          = &$this->_plugins;
        $smarty_compiler->_tpl_vars         = &$this->_tpl_vars;
        $smarty_compiler->default_modifiers = $this->default_modifiers;
        $smarty_compiler->compile_id        = $this->_compile_id;
        $smarty_compiler->_config			= $this->_config;

        $smarty_compiler->request_use_auto_globals  = $this->request_use_auto_globals;

        if ($smarty_compiler->_compile_file($tpl_file, $template_source, $template_compiled)) {
            return true;
        } else {
            $this->trigger_error($smarty_compiler->_error_msg);
            return false;
        }
    }

    /**
     * Handle modifiers
     *
     * @param string|null $modifier_name
     * @param array|null $map_array
     * @return string result of modifiers
     */
    function _run_mod_handler($_modifier_name, $_map_array, $_modifier_args)
	{
        $_func_name = $this->_plugins['modifier'][$_modifier_name][0];
		$_var = $_modifier_args[0];

        if ($_map_array && is_array($_var)) {
            foreach ($_var as $_key => $_val) {
                $_modifier_args[0] = $_val;
                $_var[$_key] = call_user_func_array($_func_name, $_modifier_args);
            }
            return $_var;
        } else {
            return call_user_func_array($_func_name, $_modifier_args);
        }
    }


    /**
     * Remove starting and ending quotes from the string
     *
     * @param string $string
     * @return string
     */    
    function _dequote($string)
    {
        if (($string{0} == "'" || $string{0} == '"') &&
            $string{strlen($string)-1} == $string{0})
            return substr($string, 1, -1);
        else
            return $string;
    }


    /**
     * read in a file from line $start for $lines.
     * read the entire file if $start and $lines are null.
     *
     * @param string $filename
     * @param integer $start
     * @param integer $lines
     * @return string
     */    
    function _read_file($filename, $start=null, $lines=null)
    {
        if (!($fd = @fopen($filename, 'r'))) {
            return false;
        }
        flock($fd, LOCK_SH);
        if ($start == null && $lines == null) {
            // read the entire file
            $contents = fread($fd, filesize($filename));
        } else {
            if ( $start > 1 ) {
                // skip the first lines before $start
                for ($loop=1; $loop < $start; $loop++) {
                    fgets($fd, 65536);
                }
            }
            if ( $lines == null ) {
                // read the rest of the file
                while (!feof($fd)) {
                    $contents .= fgets($fd, 65536);
                }
            } else {
                // read up to $lines lines
                for ($loop=0; $loop < $lines; $loop++) {
                    $contents .= fgets($fd, 65536);
                    if (feof($fd)) {
                        break;
                    }
                }
            }
        }
        fclose($fd);
        return $contents;
    }

    /**
     * get a concrete filename for automagically created content
     *
     * @param string $auto_base
     * @param string $auto_source
     * @param string $auto_id
     * @return string
     * @staticvar string|null
     * @staticvar string|null
     */    
    function _get_auto_filename($auto_base, $auto_source = null, $auto_id = null)
    {
        static $_dir_sep = null;
        static $_dir_sep_enc = null;
        
        if(!isset($_dir_sep)) {
            $_dir_sep_enc = urlencode(DIRECTORY_SEPARATOR);
            if($this->use_sub_dirs) {
                $_dir_sep = DIRECTORY_SEPARATOR;
            } else {
                $_dir_sep = '^';        
            }
        }
        
        if(@is_dir($auto_base)) {
            $_res = $auto_base . DIRECTORY_SEPARATOR;
        } else {
            // auto_base not found, try include_path
			$_params = array('file_path' => $auto_base);
            $this->_execute_core_function('get_include_path', $_params);
            $_res = isset($_params['new_file_path']) ? $_params['new_file_path'] . DIRECTORY_SEPARATOR : null;
        }
        
        if(isset($auto_id)) {
            // make auto_id safe for directory names
            $auto_id = str_replace('%7C','|',(urlencode($auto_id)));
            // split into separate directories
            $auto_id = str_replace('|', $_dir_sep, $auto_id);
            $_res .= $auto_id . $_dir_sep;
        }
        
        if(isset($auto_source)) {
            // make source name safe for filename
            if($this->use_sub_dirs) {
                $_filename = urlencode(basename($auto_source));
                $_crc32 = crc32($auto_source) . $_dir_sep;
                // prepend %% to avoid name conflicts with
                // with $auto_id names
                $_crc32 = '%%' . substr($_crc32,0,3) . $_dir_sep . '%%' . $_crc32;
                $_res .= $_crc32 . $_filename . '.php';
            } else {
                $_res .= str_replace($_dir_sep_enc,'^',urlencode($auto_source));
            }
        }
        
        return $_res;
    }

    /**
     * unlink a file, possibly using expiration time
     *
     * @param string $resource
     * @param integer $exp_time
     */    
    function _unlink($resource, $exp_time = null)
    {
        if(isset($exp_time)) {
            if(time() - filemtime($resource) >= $exp_time) {
                @unlink($resource);
            }
        } else {            
            @unlink($resource);
        }
    }

    /**
     * returns an auto_id for auto-file-functions
     *
     * @param string $cache_id
     * @param string $compile_id
     * @return string|null
     */
    function _get_auto_id($cache_id=null, $compile_id=null) {
	if (isset($cache_id))
	    return (isset($compile_id)) ? $cache_id . '|' . $compile_id  : $cache_id;
	elseif(isset($compile_id))
	    return $compile_id;
	else
	    return null;
    }

    /**
     * trigger Smarty plugin error
     *
     * @param string $error_msg
     * @param string $tpl_file
     * @param integer $tpl_line
     * @param string $file
     * @param integer $line
     * @param integer $error_type
     */    
    function _trigger_fatal_error($error_msg, $tpl_file = null, $tpl_line = null,
            $file = null, $line = null, $error_type = E_USER_ERROR)
    {
        if(isset($file) && isset($line)) {
            $info = ' ('.basename($file).", line $line)";
        } else {
            $info = null;
        }
        if (isset($tpl_line) && isset($tpl_file)) {
            trigger_error("Smarty error: [in " . $tpl_file . " line " .
                          $tpl_line . "]: $error_msg$info", $error_type);
        } else {
            trigger_error("Smarty error: $error_msg$info", $error_type);
        }
    }

    /**
     * check if the function or method exists     * @return bool
     */       
    function _plugin_implementation_exists($function)
    {
        return (is_array($function)) ?
            method_exists($function[0], $function[1]) : function_exists($function);
    }
    /**#@-*/
}

/* vim: set expandtab: */

?>

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
 * set SMARTY_DIR to absolute path to Smarty library files.
 * if not defined, include_path will be used.
 */

define('DIR_SEP', DIRECTORY_SEPARATOR);

/**
 * Sets SMARTY_DIR only if user application has not already defined it
 */
if (!defined('SMARTY_DIR')) {
    define('SMARTY_DIR', dirname(__FILE__) . DIR_SEP);
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
     * @var string
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
     *  <li >2 = use cache_lifetime in cache file</li>
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
     * This determines how Smarty handles <?php ?> tags in templates.
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
     */    
    function register_object($object, &$object_impl, $allowed = array(), $smarty_args = true)
    {
        settype($allowed, 'array');        
        settype($smarty_args, 'boolean');        
        $this->_reg_objects[$object] =
            array(&$object_impl, $allowed, $smarty_args);
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
        $this->_plugins['resource'][$type] =
            array((array)$functions, false);
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
        $this->_plugins['prefilter'][$function]
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
        $this->_plugins['postfilter'][$function]
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
        $this->_plugins['outputfilter'][$function]
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
                $this->_load_plugins(array(array($type . 'filter', $name, null, null, false)));
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
     */    
    function clear_cache($tpl_file = null, $cache_id = null, $compile_id = null, $exp_time = null)
    {
        
        if (!isset($compile_id))
            $compile_id = $this->compile_id;

	if (!isset($tpl_file))
	    $compile_id = null;

	$_auto_id = $this->_get_auto_id($cache_id, $compile_id);

        if (!empty($this->cache_handler_func)) {
            $_funcname = $this->cache_handler_func;
            return $_funcname('clear', $this, $dummy, $tpl_file, $cache_id, $compile_id);
        } else {
            return $this->_rm_auto($this->cache_dir, $tpl_file, $_auto_id, $exp_time);
        }
        
    }


    /**
     * clear the entire contents of cache (all templates)
     *
     * @param string $exp_time expire time
     */    
    function clear_all_cache($exp_time = null)
    {
        if (!empty($this->cache_handler_func)) {
            $funcname = $this->cache_handler_func;
            return $funcname('clear', $this, $dummy);
        } else {
            return $this->_rm_auto($this->cache_dir,null,null,$exp_time);
        }
    }


    /**
     * test to see if valid cache exists for this template
     *
     * @param string $tpl_file name of template file
     * @param string $cache_id
     * @param string $compile_id
     */    
    function is_cached($tpl_file, $cache_id = null, $compile_id = null)
    {
        if (!$this->caching)
            return false;

        if (!isset($compile_id))
            $compile_id = $this->compile_id;

        return $this->_read_cache_file($tpl_file, $cache_id, $compile_id, $results);
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
     */    
    function clear_compiled_tpl($tpl_file = null, $compile_id = null, $exp_time = null)
    {
        if (!isset($compile_id))
            $compile_id = $this->compile_id;
        return $this->_rm_auto($this->compile_dir, $tpl_file, $compile_id, $exp_time);
    }

    /**
     * Checks whether requested template exists.
     *
     * @param string $tpl_file
     */    
    function template_exists($tpl_file)
    {
        return $this->_fetch_template_info($tpl_file, $source, $timestamp, true, true);
    }

    /**
     * Returns an array containing template variables
     *
     * @param string $name
     * @param string $type
     * @return mixed
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
     * @return mixed
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
            $debug_start_time = $this->_get_microtime();
            $this->_smarty_debug_info[] = array('type'      => 'template',
                                                'filename'  => $tpl_file,
                                                'depth'     => 0);
            $included_tpls_idx = count($this->_smarty_debug_info) - 1;
        }

        if (!isset($compile_id)) {
            $compile_id = $this->compile_id;
        }

        $this->_compile_id = $compile_id;
        $this->_inclusion_depth = 0;

        if ($this->caching) {
            if(!empty($this->_cache_info)) {
                // nested call, init cache_info
                $_cache_info = $this->_cache_info;
                $this->_cache_info = array();
            }
            if ($this->_read_cache_file($tpl_file, $cache_id, $compile_id, $_smarty_results)) {
                if (@count($this->_cache_info['insert_tags'])) {
                    $this->_load_plugins($this->_cache_info['insert_tags']);
                    $_smarty_results = $this->_process_cached_inserts($_smarty_results);
                }
                if ($display) {
                    if ($this->debugging)
                    {
                        // capture time for debugging info
                        $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = $this->_get_microtime() - $debug_start_time;

                        $_smarty_results .= $this->_generate_debug_output();
                    }
                    if ($this->cache_modified_check) {
                        $last_modified_date = substr($GLOBALS['HTTP_SERVER_VARS']['HTTP_IF_MODIFIED_SINCE'], 0, strpos($GLOBALS['HTTP_SERVER_VARS']['HTTP_IF_MODIFIED_SINCE'], 'GMT') + 3);
                        $gmt_mtime = gmdate('D, d M Y H:i:s', $this->_cache_info['timestamp']).' GMT';
                        if (@count($this->_cache_info['insert_tags']) == 0
                            && $gmt_mtime == $last_modified_date) {
                            header("HTTP/1.1 304 Not Modified");
                        } else {
                            header("Last-Modified: ".$gmt_mtime);
                            echo $_smarty_results;
                        }
                    } else {
                            echo $_smarty_results;                        
                    }
                    error_reporting($_smarty_old_error_level);
                    return true;    
                } else {
                    error_reporting($_smarty_old_error_level);
                    return $_smarty_results;
                }
            } else {
                $this->_cache_info['template'][] = $tpl_file;
                if ($this->cache_modified_check) {
                    header("Last-Modified: ".gmdate('D, d M Y H:i:s', time()).' GMT');
                }
            }
            if(isset($_cache_info)) {
                // restore cache_info
                $this->_cache_info = $_cache_info;
            }
        }

        if (count($this->autoload_filters)) {
            $this->_autoload_filters();
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

            foreach ((array)$this->_plugins['outputfilter'] as $output_filter) {
                $_smarty_results = $output_filter[0]($_smarty_results, $this);
            }
        }

        if ($this->caching) {
            $this->_write_cache_file($tpl_file, $cache_id, $compile_id, $_smarty_results);
            $_smarty_results = $this->_process_cached_inserts($_smarty_results);
        }

        if ($display) {
            if (isset($_smarty_results)) { echo $_smarty_results; }
            if ($this->debugging) {
                // capture time for debugging info
                $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = ($this->_get_microtime() - $debug_start_time);

                echo $this->_generate_debug_output();
            }
            error_reporting($_smarty_old_error_level);
            return;
        } else {
            error_reporting($_smarty_old_error_level);
            if (isset($_smarty_results)) { return $_smarty_results; }
        }
    }


    /**
     * assign $smarty interface variable
     */    
    function _assign_smarty_interface()
    {
        if (isset($this->_smarty_vars) && isset($this->_smarty_vars['request'])) {
            return;
		}

        $globals_map = array('g'  => 'HTTP_GET_VARS',
                             'p'  => 'HTTP_POST_VARS',
                             'c'  => 'HTTP_COOKIE_VARS',
                             's'  => 'HTTP_SERVER_VARS',
                             'e'  => 'HTTP_ENV_VARS');

        $_smarty_vars_request  = array();

        foreach (preg_split('!!', strtolower($this->request_vars_order)) as $c) {
            if (isset($globals_map[$c])) {
                $_smarty_vars_request = array_merge($_smarty_vars_request, $GLOBALS[$globals_map[$c]]);
            }
        }
        $_smarty_vars_request = @array_merge($_smarty_vars_request, $GLOBALS['HTTP_SESSION_VARS']);

        $this->_smarty_vars['request'] = $_smarty_vars_request;
		
    }


    /**
     * generate debug output
     * @return string debug.tpl template output
     * @uses $debug_tpl debug template, used to display debugging output
     */
    function _generate_debug_output()
    {
        // we must force compile the debug template in case the environment
        // changed between separate applications.

        if(empty($this->debug_tpl)) {
            // set path to debug template from SMARTY_DIR
            $this->debug_tpl = 'file:'.SMARTY_DIR.'debug.tpl';
            if($this->security && is_file($this->debug_tpl)) {
                $secure_dir[] = $this->debug_tpl;
            }
        }

        $_ldelim_orig = $this->left_delimiter;
        $_rdelim_orig = $this->right_delimiter;    

        $this->left_delimiter = '{';
        $this->right_delimiter = '}';

        $_force_compile_orig = $this->force_compile;
        $this->force_compile = true;
        $_compile_id_orig = $this->_compile_id;
        $this->_compile_id = null;

        $compile_path = $this->_get_compile_path($this->debug_tpl);
        if ($this->_process_template($this->debug_tpl, $compile_path))
        {
            ob_start();
            include($compile_path);
            $results = ob_get_contents();
            ob_end_clean();
        }
        $this->force_compile = $_force_compile_orig;
        $this->_compile_id = $_compile_id_orig;

        $this->left_delimiter = $_ldelim_orig;
        $this->right_delimiter = $_rdelim_orig;

        return $results;
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
        if(@is_dir($this->config_dir)) {
            $_config_dir = $this->config_dir;            
        } else {
            // config_dir not found, try include_path
            $this->_get_include_path($this->config_dir,$_config_dir);
        }

        $_file_path = str_replace('//', '/' ,$_config_dir . '/' . $file);
        
        // get path to compiled object file
        if(isset($section)) {
               $_compile_file = $this->_get_auto_filename($this->compile_dir, $section . ' ' . $file);
        } else {
               $_compile_file = $this->_get_auto_filename($this->compile_dir, $file);
        }

        // need to compile config file?
        if($this->force_compile || !file_exists($_compile_file) ||
            ($this->compile_check &&
                file_exists($_file_path) &&
                ( filemtime($_compile_file) != filemtime($_file_path) ))) {
            $_compile_config = true;
        } else {
            include($_compile_file);
			$_compile_config = empty($_config_vars);
        }
		
        if($_compile_config) {
            if(!is_object($this->_conf_obj)) {
                require_once SMARTY_DIR . $this->config_class . '.class.php';
                $this->_conf_obj = new $this->config_class($_config_dir);
                $this->_conf_obj->overwrite = $this->config_overwrite;
                $this->_conf_obj->booleanize = $this->config_booleanize;
                $this->_conf_obj->read_hidden = $this->config_read_hidden;
                $this->_conf_obj->fix_newlines = $this->config_fix_newlines;
                $this->_conf_obj->set_path = $_config_dir;
            }
            if($_config_vars = array_merge($this->_conf_obj->get($file),
                    $this->_conf_obj->get($file, $section))) {
                if(function_exists('var_export')) {
                    $_compile_data = '<?php $_config_vars = ' . var_export($_config_vars, true) . '; return true; ?>';                    
                } else {
                    $_compile_data = '<?php $_config_vars = unserialize(\'' . str_replace('\'','\\\'', serialize($_config_vars)) . '\'); return true; ?>';
                }
                $this->_write_file($_compile_file, $_compile_data, true);
                touch($_compile_file,filemtime($_file_path));
            }
        }
        
        if ($this->debugging) {
            $debug_start_time = $this->_get_microtime();
        }

        if ($this->caching) {
            $this->_cache_info['config'][] = $file;
        }

        $this->_config[0]['vars'] = @array_merge($this->_config[0]['vars'], $_config_vars);
        $this->_config[0]['files'][$file] = true;
        
        if ($scope == 'parent') {
                $this->_config[1]['vars'] = @array_merge($this->_config[1]['vars'], $_config_vars);
                $this->_config[1]['files'][$file] = true;
        } else if ($scope == 'global') {
            for ($i = 1, $for_max = count($this->_config); $i < $for_max; $i++) {
                    $this->_config[$i]['vars'] = @array_merge($this->_config[$i]['vars'], $_config_vars);
                    $this->_config[$i]['files'][$file] = true;
            }
        }

        if ($this->debugging) {
            $debug_start_time = $this->_get_microtime();
            $this->_smarty_debug_info[] = array('type'      => 'config',
                                                'filename'  => $file.' ['.$section.'] '.$scope,
                                                'depth'     => $this->_inclusion_depth,
                                                'exec_time' => $this->_get_microtime() - $debug_start_time);
        }
    
    }

    /**
     * return a reference to a registered object
     *
     * @param string $name
     */    
	function &get_registered_object($name) {
		if (!isset($this->_reg_objects[$name]))
		$this->_trigger_fatal_error("'$name' is not a registered object");

		if (!is_object($this->_reg_objects[$name][0]))
		$this->_trigger_fatal_error("registered '$name' is not an object");

		return $this->_reg_objects[$name][0];		
	}	

    /**#@+
     * @access private
     */
    /**
     * determines if a resource is trusted or not
     *
     * @param string $resource_type
     * @param string $resource_name
     */    
    function _is_trusted($resource_type, $resource_name)
    {
        $_smarty_trusted = false;
        if ($resource_type == 'file') {
            if (!empty($this->trusted_dir)) {
                // see if template file is within a trusted directory. If so,
                // disable security during the execution of the template.

                if (!empty($this->trusted_dir)) {
                    foreach ((array)$this->trusted_dir as $curr_dir) {
                        if (!empty($curr_dir) && is_readable ($curr_dir)) {
                            if (substr(realpath($resource_name),0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
                                $_smarty_trusted = true;
                                break;
                            }
                        }
                    }
                }
            }
        } else {
            // resource is not on local file system
            $resource_func = $this->_plugins['resource'][$resource_type][0][3];
            $_smarty_trusted = $resource_func($resource_name, $this);
        }

        return $_smarty_trusted;
    }

    
    /**
     * determines if a resource is secure or not.
     *
     * @param string $resource_type
     * @param string $resource_name
     */    
    function _is_secure($resource_type, $resource_name)
    {
        if (!$this->security || $this->security_settings['INCLUDE_ANY']) {
            return true;
        }

        $_smarty_secure = false;
        if ($resource_type == 'file') {
            if (!empty($this->secure_dir)) {
                foreach ((array)$this->secure_dir as $curr_dir) {
                    if ( !empty($curr_dir) && is_readable ($curr_dir)) {
                        if (substr(realpath($resource_name),0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
                            $_smarty_secure = true;
                            break;
                        }
                    }
                }
            }
        } else {
            // resource is not on local file system
            $resource_func = $this->_plugins['resource'][$resource_type][0][2];
            $_smarty_secure = $resource_func($resource_name, $_smarty_secure, $this);
        }

        return $_smarty_secure;
    }


    /**
     * Retrieves PHP script resource
     *
     * sets $php_resource to the returned resource
     * @param string $resource
     * @param string $resource_type
     * @param  $php_resource
     */    
    function _get_php_resource($resource, &$resource_type, &$php_resource)
    {
        $this->_parse_file_path($this->trusted_dir, $resource, $resource_type, $resource_name);

        /*
         * Find out if the resource exists.
         */
        
        if ($resource_type == 'file') {
            $readable = false;
            if(file_exists($resource_name) && is_readable($resource_name)) {
                $readable = true;
            } else {
                // test for file in include_path
                if($this->_get_include_path($resource_name,$_include_path)) {
                    $readable = true;
                }
            }
        } else if ($resource_type != 'file') {
            $readable = true;
			$template_source = null;
            $resource_func = $this->_plugins['resource'][$resource_type][0][0];
            $readable = $resource_func($resource_name, $template_source, $this);
        }

        /*
         * Set the error function, depending on which class calls us.
         */
        if (method_exists($this, '_syntax_error')) {
            $error_func = '_syntax_error';
        } else {
            $error_func = 'trigger_error';
        }

        if ($readable) {
            if ($this->security) {
                if (!$this->_is_trusted($resource_type, $resource_name)) {
                    $this->$error_func("(secure mode) '$resource_type:$resource_name' is not trusted");
                    return false;
                }
            }
        } else {
            $this->$error_func("'$resource_type: $resource_name' is not readable");
            return false;
        }

        if ($resource_type == 'file') {
            $php_resource = $resource_name;
        } else {
            $php_resource = $template_source;
        }

        return true;
    }


    /**
     * umm... process the template
     *
     * @param string $tpl_file
     * @param string $compile_path
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
                if (!$this->_fetch_template_info($tpl_file, $template_source,
                                                 $template_timestamp)) {
                    return false;
                }
                if ($template_timestamp <= filemtime($compile_path)) {
                    // template not expired, no recompile
                    return true;
                } else {
                    // compile template
                    $this->_compile_template($tpl_file, $template_source, $template_compiled);
                    $this->_write_compiled_template($compile_path, $template_compiled, $template_timestamp);
                    return true;
                }
            }
        } else {
            // compiled template does not exist, or forced compile
            if (!$this->_fetch_template_info($tpl_file, $template_source,
                                             $template_timestamp)) {
                return false;
            }
            $this->_compile_template($tpl_file, $template_source, $template_compiled);
            $this->_write_compiled_template($compile_path, $template_compiled, $template_timestamp);
            return true;
        }
    }

    /**
     * Get the compile path for this template file
     *
     * @param string $tpl_file
     */    
    function _get_compile_path($tpl_file)
    {
        return $this->_get_auto_filename($this->compile_dir, $tpl_file,
                                         $this->_compile_id);
    }

   /**
     * write the compiled template
     *
     * @param string $compile_path
     * @param string $template_compiled
     * @param integer $template_timestamp
     * @return true
     */    
    function _write_compiled_template($compile_path, $template_compiled, $template_timestamp)
    {
        // we save everything into $compile_dir
        $this->_write_file($compile_path, $template_compiled, true);
        touch($compile_path, $template_timestamp);
        return true;
    }

    /**
     * parse out the type and name from the template resource
     *
     * @param string $file_base_path
     * @param string $file_path
     * @param string $resource_type
     * @param string $resource_name
     */    
    function _parse_file_path($file_base_path, $file_path, &$resource_type, &$resource_name)
    {
        // split tpl_path by the first colon
        $_file_path_parts = explode(':', $file_path, 2);

        if (count($_file_path_parts) == 1) {
            // no resource type, treat as type "file"
            $resource_type = 'file';
            $resource_name = $_file_path_parts[0];
        } else {
            $resource_type = $_file_path_parts[0];
            $resource_name = $_file_path_parts[1];
            if ($resource_type != 'file') {
                $this->_load_resource_plugin($resource_type);
            }
        }

        if ($resource_type == 'file') {
            if (!preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $resource_name)) {
                // relative pathname to $file_base_path
                // use the first directory where the file is found
                foreach ((array)$file_base_path as $_curr_path) {
                    $_fullpath = $_curr_path . DIR_SEP . $resource_name;
                    if (file_exists($_fullpath) && is_file($_fullpath)) {
                        $resource_name = $_fullpath;
                        return true;
                    }
                    // didn't find the file, try include_path
                    if($this->_get_include_path($_fullpath, $_include_path)) {
                        $resource_name = $_include_path;
                        return true;
                    }
                }
                return false;
            }
        }

        // resource type != file
        return true;
    }


    /**
     * fetch the template info. Gets timestamp, and source
     * if get_source is true
     *
     * sets $template_source to the source of the template, and
     * $template_timestamp to its time stamp
     * @param string $tpl_path
     * @param string $template_source
     * @param integer $template_timestamp
     * @param boolean $get_source
     * @param boolean $quiet
     */    
    function _fetch_template_info($tpl_path, &$template_source, &$template_timestamp, $get_source = true, $quiet = false)
    {
        $_return = false;
        if ($this->_parse_file_path($this->template_dir, $tpl_path, $resource_type, $resource_name)) {
            switch ($resource_type) {
                case 'file':
                    if ($get_source) {
                        $template_source = $this->_read_file($resource_name);
                    }
                    $template_timestamp = filemtime($resource_name);
                    $_return = true;
                    break;

                default:
                    // call resource functions to fetch the template source and timestamp
                    if ($get_source) {
                        $resource_func = $this->_plugins['resource'][$resource_type][0][0];
                        $_source_return = $resource_func($resource_name, $template_source, $this);
                    } else {
                        $_source_return = true;
                    }
                    $resource_func = $this->_plugins['resource'][$resource_type][0][1];
                    $_timestamp_return = $resource_func($resource_name, $template_timestamp, $this);
                    $_return = $_source_return && $_timestamp_return;
                    break;
            }
        }
        
        if (!$_return) {
            // see if we can get a template with the default template handler
            if (!empty($this->default_template_handler_func)) {
                if (!function_exists($this->default_template_handler_func)) {
                    $this->trigger_error("default template handler function \"$this->default_template_handler_func\" doesn't exist.");
                } else {
                	$funcname = $this->default_template_handler_func;
                	$_return = $funcname($resource_type, $resource_name, $template_source, $template_timestamp, $this);
				}
            }
        }

        if (!$_return) {
            if (!$quiet) {
                $this->trigger_error("unable to read template resource: \"$tpl_path\"");
            }
        } else if ($_return && $this->security && !$this->_is_secure($resource_type, $resource_name)) {
            if (!$quiet)
                $this->trigger_error("(secure mode) accessing \"$tpl_path\" is not allowed");
            $template_source = null;
            $template_timestamp = null;
            return false;
        }

        return $_return;
    }


    /**
     * called to compile the templates
     *
     * sets $template_compiled to the compiled template
     * @param string $tpl_file
     * @param string $template_source
     * @param string $template_compiled
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

        if ($smarty_compiler->_compile_file($tpl_file, $template_source, $template_compiled)) {
            return true;
        } else {
            $this->trigger_error($smarty_compiler->_error_msg);
            return false;
        }
    }

    /**
     * called for included templates
     *
     * @param string $_smarty_include_tpl_file
     * @param string $_smarty_include_vars
     */    
    function _smarty_include($_smarty_include_tpl_file, $_smarty_include_vars)
    {
        if ($this->debugging) {
            $debug_start_time = $this->_get_microtime();
            $this->_smarty_debug_info[] = array('type'      => 'template',
                                                'filename'  => $_smarty_include_tpl_file,
                                                'depth'     => ++$this->_inclusion_depth);
            $included_tpls_idx = count($this->_smarty_debug_info) - 1;
        }

        $this->_tpl_vars = array_merge($this->_tpl_vars, $_smarty_include_vars);

        // config vars are treated as local, so push a copy of the
        // current ones onto the front of the stack
        array_unshift($this->_config, $this->_config[0]);

        $_smarty_compile_path = $this->_get_compile_path($_smarty_include_tpl_file);

        if ($this->_process_template($_smarty_include_tpl_file, $_smarty_compile_path)) {
            include($_smarty_compile_path);
        }

        // pop the local vars off the front of the stack
        array_shift($this->_config);

        $this->_inclusion_depth--;

        if ($this->debugging) {
            // capture time for debugging info
            $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = $this->_get_microtime() - $debug_start_time;
        }

        if ($this->caching) {
            $this->_cache_info['template'][] = $_smarty_include_tpl_file;
        }
    }

    /**
     * called for included templates
     *
     * @param string $_smarty_include_php_file
     * @param string $_smarty_assign variable to assign the included template's
     *               output into
     * @param boolean $_smarty_once uses include_once if this is true
     * @param array $_smarty_include_vars associative array of vars from
     *              {include file="blah" var=$var}
     */    
    function _smarty_include_php($_smarty_include_php_file, $_smarty_assign, $_smarty_once, $_smarty_include_vars)
    {
        $this->_get_php_resource($_smarty_include_php_file, $_smarty_resource_type,
                                 $_smarty_php_resource);

        extract($_smarty_include_vars, EXTR_PREFIX_SAME, 'include_php_');

        if (!empty($_smarty_assign)) {
            ob_start();
            if ($_smarty_resource_type == 'file') {
                if($_smarty_once) {
                    include_once($_smarty_php_resource);
                } else {
                    include($_smarty_php_resource);                    
                }
            } else {
                eval($_smarty_php_resource);
            }
            $this->assign($_smarty_assign, ob_get_contents());
            ob_end_clean();
        } else {
            if ($_smarty_resource_type == 'file') {
                if($_smarty_once) {
                    include_once($_smarty_php_resource);
                } else {
                    include($_smarty_php_resource);                    
                }
            } else {
                eval($_smarty_php_resource);
            }
        }
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
     * Replace cached inserts with the actual results
     *
     * @param string $results
     */    
    function _process_cached_inserts($results)
    {
        preg_match_all('!'.$this->_smarty_md5.'{insert_cache (.*)}'.$this->_smarty_md5.'!Uis',
                       $results, $match);
        list($cached_inserts, $insert_args) = $match;

        for ($i = 0, $for_max = count($cached_inserts); $i < $for_max; $i++) {
            if ($this->debugging) {
                $debug_start_time = $this->_get_microtime();
            }

            $args = unserialize($insert_args[$i]);
            $name = $args['name'];

            if (isset($args['script'])) {
                if (!$this->_get_php_resource($this->_dequote($args['script']), $resource_type, $php_resource)) {
                    return false;
                }

                if ($resource_type == 'file') {
                    include_once($php_resource);
                } else {
                    eval($php_resource);
                }
            }

            $function_name = $this->_plugins['insert'][$name][0];
            $replace = $function_name($args, $this);

            $results = str_replace($cached_inserts[$i], $replace, $results);
            if ($this->debugging) {
                $this->_smarty_debug_info[] = array('type'      => 'insert',
                                                    'filename'  => 'insert_'.$name,
                                                    'depth'     => $this->_inclusion_depth,
                                                    'exec_time' => $this->_get_microtime() - $debug_start_time);
            }
        }

        return $results;
    }


    /**
     * Handle insert tags
     *
     * @param array $args
     */    
    function _run_insert_handler($args)
    {
        if ($this->debugging) {
            $debug_start_time = $this->_get_microtime();
        }
    
        if ($this->caching) {
            $arg_string = serialize($args);
            $name = $args['name'];
            if (!isset($this->_cache_info['insert_tags'][$name])) {
                $this->_cache_info['insert_tags'][$name] = array('insert',
                                                                 $name,
                                                                 $this->_plugins['insert'][$name][1],
                                                                 $this->_plugins['insert'][$name][2],
                                                                 !empty($args['script']) ? true : false);
            }
            return $this->_smarty_md5."{insert_cache $arg_string}".$this->_smarty_md5;
        } else {
            if (isset($args['script'])) {
                if (!$this->_get_php_resource($this->_dequote($args['script']), $resource_type, $php_resource)) {
                    return false;
                }
    
                if ($resource_type == 'file') {
                    include_once($php_resource);
                } else {
                    eval($php_resource);
                }
                unset($args['script']);
            }
    
            $function_name = $this->_plugins['insert'][$args['name']][0];
            $content = $function_name($args, $this);
            if ($this->debugging) {
                $this->_smarty_debug_info[] = array('type'      => 'insert',
                                                    'filename'  => 'insert_'.$args['name'],
                                                    'depth'     => $this->_inclusion_depth,
                                                    'exec_time' => $this->_get_microtime() - $debug_start_time);
            }
    
            if (!empty($args["assign"])) {
                $this->assign($args["assign"], $content);
            } else {
                return $content;
            }
        }
    }


    /**
     * Handle modifiers
     *
     */
    function _run_mod_handler()
    {
        $args = func_get_args();
        list($modifier_name, $map_array) = array_splice($args, 0, 2);
        list($func_name, $tpl_file, $tpl_line) =
            $this->_plugins['modifier'][$modifier_name];
        $var = $args[0];

        if ($map_array && is_array($var)) {
            foreach ($var as $key => $val) {
                $args[0] = $val;
                $var[$key] = call_user_func_array($func_name, $args);
            }
            return $var;
        } else {
            return call_user_func_array($func_name, $args);
        }
    }


    /**
     * Remove starting and ending quotes from the string
     *
     * @param string $string
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
     * write out a file to disk
     *
     * @param string $filename
     * @param string $contents
     * @param boolean $create_dirs
     */    
    function _write_file($filename, $contents, $create_dirs = false)
    {
		$_dirname = dirname($filename);
		
        if ($create_dirs) {
            $this->_create_dir_structure($_dirname);
		}

		// write to tmp file, then rename it to avoid
		// file locking race condition
		$_tmp_file = $_dirname . '/' . uniqid('');
		
        if (!($fd = @fopen($_tmp_file, 'w'))) {
            $this->trigger_error("problem writing temporary file '$_tmp_file'");
            return false;
        }

        fwrite($fd, $contents);
        fclose($fd);
		// Win32 can't rename over top another file
		if(strtoupper(substr(PHP_OS, 0, 3)) == 'WIN' && file_exists($filename)) {
			@unlink($filename);
		} 
		@rename($_tmp_file, $filename);
        chmod($filename, $this->_file_perms);

        return true;
    }

    /**
     * get a concrete filename for automagically created content
     *
     * @param string $auto_base
     * @param string $auto_source
     * @param string $auto_id
     */    
    function _get_auto_filename($auto_base, $auto_source = null, $auto_id = null)
    {
        static $_dir_sep = null;
        static $_dir_sep_enc = null;
        
        if(!isset($_dir_sep)) {
            $_dir_sep_enc = urlencode(DIR_SEP);
            if($this->use_sub_dirs) {
                $_dir_sep = DIR_SEP;
            } else {
                $_dir_sep = '^';        
            }
        }
        
        if(@is_dir($auto_base)) {
            $res = $auto_base . DIR_SEP;
        } else {
            // auto_base not found, try include_path
            $this->_get_include_path($auto_base,$_include_path);
            $res = $_include_path . DIR_SEP;
        }
        
        if(isset($auto_id)) {
            // make auto_id safe for directory names
            $auto_id = str_replace('%7C','|',(urlencode($auto_id)));
            // split into separate directories
            $auto_id = str_replace('|', $_dir_sep, $auto_id);
            $res .= $auto_id . $_dir_sep;
        }
        
        if(isset($auto_source)) {
            // make source name safe for filename
            if($this->use_sub_dirs) {
                $_filename = urlencode(basename($auto_source));
                $_crc32 = crc32($auto_source) . $_dir_sep;
                // prepend %% to avoid name conflicts with
                // with $auto_id names
                $_crc32 = '%%' . substr($_crc32,0,3) . $_dir_sep . '%%' . $_crc32;
                $res .= $_crc32 . $_filename . '.php';
            } else {
                $res .= str_replace($_dir_sep_enc,'^',urlencode($auto_source));
            }
        }
        
        return $res;
    }

    /**
     * delete an automagically created file by name and id
     *
     * @param string $auto_base
     * @param string $auto_source
     * @param string $auto_id
     * @param integer $exp_time
     */    
    function _rm_auto($auto_base, $auto_source = null, $auto_id = null, $exp_time = null)
    {
        if (!@is_dir($auto_base))
          return false;

        if(!isset($auto_id) && !isset($auto_source)) {
            $res = $this->_rmdir($auto_base, 0, $exp_time);            
        } else {        
            $tname = $this->_get_auto_filename($auto_base, $auto_source, $auto_id);
            
            if(isset($auto_source)) {
                $res = $this->_unlink($tname);
            } elseif ($this->use_sub_dirs) {
                $res = $this->_rmdir($tname, 1, $exp_time);
            } else {
                // remove matching file names
                $handle = opendir($auto_base);
		$res = true;
                while (false !== ($filename = readdir($handle))) {
                    if($filename == '.' || $filename == '..') {
                        continue;    
                    } elseif (substr($auto_base . DIR_SEP . $filename,0,strlen($tname)) == $tname) {
                        $res &= (bool)$this->_unlink($auto_base . DIR_SEP . $filename, $exp_time);
                    }
                }
            }
        }

        return $res;
    }

    /**
     * delete a dir recursively (level=0 -> keep root)
     * WARNING: no tests, it will try to remove what you tell it!
     *
     * @param string $dirname
     * @param integer $level
     * @param integer $exp_time
     */    
    function _rmdir($dirname, $level = 1, $exp_time = null)
    {

       if($handle = @opendir($dirname)) {

            while (false !== ($entry = readdir($handle))) {
                if ($entry != '.' && $entry != '..') {
                    if (@is_dir($dirname . DIR_SEP . $entry)) {
                        $this->_rmdir($dirname . DIR_SEP . $entry, $level + 1, $exp_time);
                    }
                    else {
                        $this->_unlink($dirname . DIR_SEP . $entry, $exp_time);
                    }
                }
            }

            closedir($handle);

            if ($level)
                @rmdir($dirname);
            
            return true;
        
        } else {
                return false;
        }
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
     * create full directory structure
     *
     * @param string $dir
     */    
    function _create_dir_structure($dir)
    {
        if (!file_exists($dir)) {
            $_dir_parts = preg_split('!\\'.DIR_SEP.'+!', $dir, -1, PREG_SPLIT_NO_EMPTY);
            $_new_dir = ($dir{0} == DIR_SEP) ? DIR_SEP : '';
            
            // do not attempt to test or make directories outside of open_basedir
            $_open_basedir_ini = ini_get('open_basedir');
            if(!empty($_open_basedir_ini)) {
                $_use_open_basedir = true;
                $_open_basedir_sep = (strtoupper(substr(PHP_OS, 0, 3)) == 'WIN') ? ';' : ':';
                $_open_basedirs = explode($_open_basedir_sep, $_open_basedir_ini);
            } else {                    
                $_use_open_basedir = false;
            }

            foreach ($_dir_parts as $_dir_part) {
                $_new_dir .= $_dir_part;

                if ($_use_open_basedir) {
                    $_make_new_dir = false;
                    foreach ($_open_basedirs as $_open_basedir) {
                        if (substr($_new_dir.'/', 0, strlen($_open_basedir)) == $_open_basedir) {
                            $_make_new_dir = true;
                            break;
                        }
                    }
                } else {
                    $_make_new_dir = true;                    
                }

                if ($_make_new_dir && !file_exists($_new_dir) && !@mkdir($_new_dir, $this->_dir_perms)) {
                    $this->trigger_error("problem creating directory \"$dir\"");
                    return false;
                }
                $_new_dir .= DIR_SEP;
            }
        }
    }

    /**
     * Prepend the cache information to the cache file
     * and write it
     *
     * @param string $tpl_file
     * @param string $cache_id
     * @param string $compile_id
     * @param string $results
     */    
    function _write_cache_file($tpl_file, $cache_id, $compile_id, $results)
    {
        // put timestamp in cache header
        $this->_cache_info['timestamp'] = time();
        if ($this->cache_lifetime > -1){
            // expiration set
            $this->_cache_info['expires'] = $this->_cache_info['timestamp'] + $this->cache_lifetime;
        } else {
            // cache will never expire
            $this->_cache_info['expires'] = -1;
        }

        // prepend the cache header info into cache file
        $results = serialize($this->_cache_info)."\n".$results;

        if (!empty($this->cache_handler_func)) {
            // use cache_handler function
            $_funcname = $this->cache_handler_func;
            return $_funcname('write', $this, $results, $tpl_file, $cache_id, $compile_id);
        } else {
            // use local cache file
            $_auto_id = $this->_get_auto_id($cache_id, $compile_id);
            $_cache_file = $this->_get_auto_filename($this->cache_dir, $tpl_file, $_auto_id);
            $this->_write_file($_cache_file, $results, true);
            return true;
        }
    }

    /**
     * read a cache file, determine if it needs to be
     * regenerated or not
     *
     * @param string $tpl_file
     * @param string $cache_id
     * @param string $compile_id
     * @param string $results
     */    
    function _read_cache_file($tpl_file, $cache_id, $compile_id, &$results)
    {
        static  $content_cache = array();

        if ($this->force_compile) {
            // force compile enabled, always regenerate
            return false;
        }

        if (isset($content_cache["$tpl_file,$cache_id,$compile_id"])) {
            list($results, $this->_cache_info) = $content_cache["$tpl_file,$cache_id,$compile_id"];
            return true;
        }

        if (!empty($this->cache_handler_func)) {
            // use cache_handler function
            $_funcname = $this->cache_handler_func;
            $_funcname('read', $this, $results, $tpl_file, $cache_id, $compile_id);
        } else {
            // use local cache file
            $_auto_id = $this->_get_auto_id($cache_id, $compile_id);
            $_cache_file = $this->_get_auto_filename($this->cache_dir, $tpl_file, $_auto_id);
            $results = $this->_read_file($_cache_file);
        }

        if (empty($results)) {
            // nothing to parse (error?), regenerate cache
            return false;
        }

        $cache_split = explode("\n", $results, 2);
        $cache_header = $cache_split[0];

        $this->_cache_info = unserialize($cache_header);

        if ($this->caching == 2 && isset ($this->_cache_info['expires'])){
            // caching by expiration time
            if ($this->_cache_info['expires'] > -1 && (time() > $this->_cache_info['expires'])) {
            // cache expired, regenerate
            return false;
            }
        } else {
            // caching by lifetime
            if ($this->cache_lifetime > -1 && (time() - $this->_cache_info['timestamp'] > $this->cache_lifetime)) {
            // cache expired, regenerate
            return false;
            }
        }

        if ($this->compile_check) {
            foreach ($this->_cache_info['template'] as $template_dep) {
                $this->_fetch_template_info($template_dep, $template_source, $template_timestamp, false);
                if ($this->_cache_info['timestamp'] < $template_timestamp) {
                    // template file has changed, regenerate cache
                    return false;
                }
            }

            if (isset($this->_cache_info['config'])) {
                foreach ($this->_cache_info['config'] as $config_dep) {
                    if ($this->_cache_info['timestamp'] < filemtime($this->config_dir.DIR_SEP.$config_dep)) {
                        // config file has changed, regenerate cache
                        return false;
                    }
                }
            }
        }

        $results = $cache_split[1];
        $content_cache["$tpl_file,$cache_id,$compile_id"] = array($results, $this->_cache_info);

        return true;
    }

    /**
     * returns an auto_id for auto-file-functions
     *
     * @param string $cache_id
     * @param string $compile_id
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
     * get filepath of requested plugin
     *
     * @param string $type
     * @param string $name
     */    
    function _get_plugin_filepath($type, $name)
    {
        $_plugin_filename = "$type.$name.php";
        
        foreach ((array)$this->plugins_dir as $_plugin_dir) {

            $_plugin_filepath = $_plugin_dir . DIR_SEP . $_plugin_filename;

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

                $_plugin_filepath = $_plugin_dir . DIR_SEP . $_plugin_filename;

                if ($this->_get_include_path($_plugin_filepath, $_include_filepath)) {
                    return $_include_filepath;
                }
            }
        }
        
        
        return false;
    }

    /**
     * Load requested plugins
     *
     * @param array $plugins
     */    
    function _load_plugins($plugins)
    {
        
        foreach ($plugins as $plugin_info) {            
            list($type, $name, $tpl_file, $tpl_line, $delayed_loading) = $plugin_info;
            $plugin = &$this->_plugins[$type][$name];
            
            /*
             * We do not load plugin more than once for each instance of Smarty.
             * The following code checks for that. The plugin can also be
             * registered dynamically at runtime, in which case template file
             * and line number will be unknown, so we fill them in.
             *
             * The final element of the info array is a flag that indicates
             * whether the dynamically registered plugin function has been
             * checked for existence yet or not.
             */
            if (isset($plugin)) {
                if (!$plugin[3]) {
                    if (!function_exists($plugin[0])) {
                        $this->_trigger_fatal_error("[plugin] $type '$name' is not implemented", $tpl_file, $tpl_line, __FILE__, __LINE__);
                    } else {
                        $plugin[1] = $tpl_file;
                        $plugin[2] = $tpl_line;
                        $plugin[3] = true;
                    }
                }
                continue;
            } else if ($type == 'insert') {
                /*
                 * For backwards compatibility, we check for insert functions in
                 * the symbol table before trying to load them as a plugin.
                 */
                $plugin_func = 'insert_' . $name;
                if (function_exists($plugin_func)) {
                    $plugin = array($plugin_func, $tpl_file, $tpl_line, true);
                    continue;
                }
            }

            $plugin_file = $this->_get_plugin_filepath($type, $name);

            if (! $found = ($plugin_file != false)) {
                $message = "could not load plugin file '$type.$name.php'\n";
            }

            /*
             * If plugin file is found, it -must- provide the properly named
             * plugin function. In case it doesn't, simply output the error and
             * do not fall back on any other method.
             */
            if ($found) {
                include_once $plugin_file;

                $plugin_func = 'smarty_' . $type . '_' . $name;
                if (!function_exists($plugin_func)) {
                    $this->_trigger_fatal_error("[plugin] function $plugin_func() not found in $plugin_file", $tpl_file, $tpl_line, __FILE__, __LINE__);
                    continue;
                }
            }
            /*
             * In case of insert plugins, their code may be loaded later via
             * 'script' attribute.
             */
            else if ($type == 'insert' && $delayed_loading) {
                $plugin_func = 'smarty_' . $type . '_' . $name;
                $found = true;
            }

            /*
             * Plugin specific processing and error checking.
             */
            if (!$found) {
                if ($type == 'modifier') {
                    /*
                     * In case modifier falls back on using PHP functions
                     * directly, we only allow those specified in the security
                     * context.
                     */
                    if ($this->security && !in_array($name, $this->security_settings['MODIFIER_FUNCS'])) {
                        $message = "(secure mode) modifier '$name' is not allowed";
                    } else {
                        if (!function_exists($name)) {
                            $message = "modifier '$name' is not implemented";
                        } else {
                            $plugin_func = $name;
                            $found = true;
                        }
                    }
                } else if ($type == 'function') {
                    /*
                     * This is a catch-all situation.
                     */
                    $message = "unknown tag - '$name'";
                }
            }

            if ($found) {
                $this->_plugins[$type][$name] = array($plugin_func, $tpl_file, $tpl_line, true);
            } else {
                // output error
                $this->_trigger_fatal_error('[plugin] ' . $message, $tpl_file, $tpl_line, __FILE__, __LINE__);
            }
        }
    }

    /**
     * load a resource plugin
     *
     * @param string $type
     */    
    function _load_resource_plugin($type)
    {
        /*
         * Resource plugins are not quite like the other ones, so they are
         * handled differently. The first element of plugin info is the array of
         * functions provided by the plugin, the second one indicates whether
         * all of them exist or not.
         */

        $plugin = &$this->_plugins['resource'][$type];
        if (isset($plugin)) {
            if (!$plugin[1] && count($plugin[0])) {
                $plugin[1] = true;
                foreach ($plugin[0] as $plugin_func) {
                    if (!function_exists($plugin_func)) {
                        $plugin[1] = false;
                        break;
                    }
                }
            }

            if (!$plugin[1]) {
                $this->_trigger_fatal_error("[plugin] resource '$type' is not implemented", null, null, __FILE__, __LINE__);
            }

            return;
        }

        $plugin_file = $this->_get_plugin_filepath('resource', $type);
        $found = ($plugin_file != false);

        if ($found) {            /*
             * If the plugin file is found, it -must- provide the properly named
             * plugin functions.
             */
            include_once $plugin_file;

            /*
             * Locate functions that we require the plugin to provide.
             */
            $resource_ops = array('source', 'timestamp', 'secure', 'trusted');
            $resource_funcs = array();
            foreach ($resource_ops as $op) {
                $plugin_func = 'smarty_resource_' . $type . '_' . $op;
                if (!function_exists($plugin_func)) {
                    $this->_trigger_fatal_error("[plugin] function $plugin_func() not found in $plugin_file", null, null, __FILE__, __LINE__);
                    return;
                } else {
                    $resource_funcs[] = $plugin_func;
                }
            }

            $this->_plugins['resource'][$type] = array($resource_funcs, true);
        }
    }

    /**
     * automatically load a set of filters
     */
    function _autoload_filters()
    {
        foreach ($this->autoload_filters as $filter_type => $filters) {
            foreach ($filters as $filter) {
                $this->load_filter($filter_type, $filter);
            }
        }
    }

    /**
     * Quote subpattern references
     *
     * @param string $string
     */
    function quote_replace($string)
    {
        return preg_replace('![\\$]\d!', '\\\\\\0', $string);
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
     * Get seconds and microseconds
     * @return double
     */    
    function _get_microtime()
    {
        $mtime = microtime();
        $mtime = explode(" ", $mtime);
        $mtime = (double)($mtime[1]) + (double)($mtime[0]);
        return ($mtime);
    }

    /**
     * Get path to file from include_path
     *
     * @param string $file_path
     * @param string $new_file_path
     */    
    function _get_include_path($file_path, &$new_file_path)
    {
        static $_path_array = null;
        
        if(!isset($_path_array)) {
            $_ini_include_path = ini_get('include_path');

            if(strstr($_ini_include_path,';')) {
                // windows pathnames
                $_path_array = explode(';',$_ini_include_path);
            } else {
                $_path_array = explode(':',$_ini_include_path);
            }
        }
        foreach ($_path_array as $_include_path) {
            if (file_exists($_include_path . DIR_SEP . $file_path)) {
                   $new_file_path = $_include_path . DIR_SEP . $file_path;
                return true;
            }
        }
        return false;
    }    
    /**#@-*/
}

/* vim: set expandtab: */

?>

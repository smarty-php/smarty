<?php

/*
 * Project:     Smarty: the PHP compiling template engine
 * File:        Smarty.class.php
 * Author:      Monte Ohrt <monte@ispi.net>
 *              Andrei Zmievski <andrei@php.net>
 *
 * Version:     1.5.1
 * Copyright:   2001 ispi of Lincoln, Inc.
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
 * Smarty mailing list. Send a blank e-mail to smarty-subscribe@lists.ispi.net
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
 * http://www.phpinsider.com/
 *
 */

// set SMARTY_DIR to absolute path to Smarty library files.
// if not defined, include_path will be used.

if (!defined('SMARTY_DIR')) {
    define('SMARTY_DIR', '');
}

require_once SMARTY_DIR.'Smarty.addons.php';

define('SMARTY_PHP_PASSTHRU',   0);
define('SMARTY_PHP_QUOTE',      1);
define('SMARTY_PHP_REMOVE',     2);
define('SMARTY_PHP_ALLOW',      3);

class Smarty
{

/**************************************************************************/
/* BEGIN SMARTY CONFIGURATION SECTION                                     */
/* Set the following config variables to your liking.                     */
/**************************************************************************/

    // public vars
    var $template_dir    =  './templates';     // name of directory for templates
    var $compile_dir     =  './templates_c';   // name of directory for compiled templates
    var $config_dir      =  './configs';       // directory where config files are located

    var $debugging       =  false;             // enable debugging console true/false
    var $debug_tpl       =  'file:debug.tpl';  // path to debug console template
    var $debugging_ctrl  =  'NONE';            // Possible values:
                                               // NONE - no debug control allowed
                                               // URL - enable debugging when keyword
                                               //       SMARTY_DEBUG is found in $QUERY_STRING

    var $global_assign   =  array( 'HTTP_SERVER_VARS' => array( 'SCRIPT_NAME' )
                                 );     // variables from the GLOBALS array
                                        // that are implicitly assigned
                                        // to all templates
    var $undefined       =  null;       // undefined variables in $global_assign will be
                                        // created with this value
    var $compile_check   =  true;       // whether to check for compiling step or not:
                                        // This is generally set to false once the
                                        // application is entered into production and
                                        // initially compiled. Leave set to true
                                        // during development. true/false default true.

    var $force_compile   =  false;      // force templates to compile every time,
                                        // overrides cache settings. default false.

    var $caching         =  false;      // enable caching. true/false default false.
    var $cache_dir       =  './cache';  // name of directory for template cache files
    var $cache_lifetime  =  3600;       // number of seconds cached content will persist.
                                        // 0 = never expires. default is one hour (3600)
    var $insert_tag_check    = true;    // if you have caching turned on and you
                                        // don't use {insert} tags anywhere
                                        // in your templates, set this to false.
                                        // this will tell Smarty not to look for
                                        // insert tags, thus speeding up cached page
                                        // fetches. true/false default true.
    var $cache_handler_func   = null;   // function used for cached content. this is
                                        // an alternative to using the built-in file
										// based caching.

	
	var $default_template_handler_func = ''; // function to handle missing templates

    var $tpl_file_ext    =  '.tpl';     // template file extention (deprecated)

    var $php_handling    =  SMARTY_PHP_PASSTHRU;
                                        // how smarty handles php tags in the templates
                                        // possible values:
                                        // SMARTY_PHP_PASSTHRU -> echo tags as is
                                        // SMARTY_PHP_QUOTE    -> escape tags as entities
                                        // SMARTY_PHP_REMOVE   -> remove php tags
                                        // SMARTY_PHP_ALLOW    -> execute php tags
                                        // default: SMARTY_PHP_PASSTHRU


    var $security       =   false;      // enable template security (default false)
    var $secure_dir     =   array('./templates'); // array of directories considered secure
    var $security_settings  = array(
                                    'PHP_HANDLING'    => false,
                                    'IF_FUNCS'        => array('array', 'list',
                                                               'isset', 'empty',
                                                               'count', 'sizeof',
                                                               'in_array', 'is_array'),
                                    'INCLUDE_ANY'     => false,
                                    'PHP_TAGS'        => false,
                                    'MODIFIER_FUNCS'  => array('count')
                                   );
	var $trusted_dir		= array(); 	// directories where trusted templates
									// reside ($security is disabled during their
									// execution).

    var $left_delimiter  =  '{';        // template tag delimiters.
    var $right_delimiter =  '}';

    var $compiler_funcs  =  array(
                                 );

    var $custom_funcs    =  array(  'html_options'      => 'smarty_func_html_options',
                                    'html_select_date'  => 'smarty_func_html_select_date',
                                    'html_select_time'  => 'smarty_func_html_select_time',
                                    'math'              => 'smarty_func_math',
                                    'fetch'             => 'smarty_func_fetch',
                                    'counter'           => 'smarty_func_counter',
                                    'assign'            => 'smarty_func_assign',
                                    'popup_init'        => 'smarty_func_overlib_init',
                                    'popup'             => 'smarty_func_overlib',
                                    'assign_debug_info' => 'smarty_func_assign_debug_info'
                                 );

    var $custom_mods     =  array(  'lower'             => 'strtolower',
                                    'upper'             => 'strtoupper',
                                    'capitalize'        => 'ucwords',
                                    'escape'            => 'smarty_mod_escape',
                                    'truncate'          => 'smarty_mod_truncate',
                                    'spacify'           => 'smarty_mod_spacify',
                                    'date_format'       => 'smarty_mod_date_format',
                                    'string_format'     => 'smarty_mod_string_format',
                                    'replace'           => 'smarty_mod_replace',
                                    'regex_replace'     => 'smarty_mod_regex_replace',
                                    'strip_tags'        => 'smarty_mod_strip_tags',
                                    'default'           => 'smarty_mod_default',
                                    'count_characters'  => 'smarty_mod_count_characters',
                                    'count_words'       => 'smarty_mod_count_words',
                                    'count_sentences'   => 'smarty_mod_count_sentences',
                                    'count_paragraphs'  => 'smarty_mod_count_paragraphs',
                                    'debug_print_var'   => 'smarty_mod_debug_print_var'
                                 );

    var $show_info_header      =   false;     // display HTML info header at top of page output
    var $show_info_include     =   false;      // display HTML comments at top & bottom of
                                              // each included template

    var $compiler_class        =   'Smarty_Compiler'; // the compiler class used by
                                                      // Smarty to compile templates
    var $resource_funcs        =  array();  // functions that resource handlers are mapped to
    var $prefilter_funcs       =  array();  // functions that templates are filtered through
                                            // before being compiled
    var $postfilter_funcs      =  array();  // functions that compiled templates are filtered
                                            // through after compilation

    var $request_vars_order    = "EGPCS";   // the order in which request variables are
                                            // registered, similar to variables_order
                                            // in php.ini

    var $compile_id            = null;      // persistent compile identifier

/**************************************************************************/
/* END SMARTY CONFIGURATION SECTION                                       */
/* There should be no need to touch anything below this line.             */
/**************************************************************************/

    // internal vars
    var $_error_msg            =   false;      // error messages. true/false
    var $_tpl_vars             =   array();    // where assigned template vars are kept
    var $_smarty_vars          =   array();    // stores run-time $smarty.* vars
    var $_sections             =   array();    // keeps track of sections
    var $_foreach              =   array();    // keeps track of foreach blocks
    var $_conf_obj             =   null;       // configuration object
    var $_config               =   array();    // loaded configuration settings
    var $_smarty_md5           =   'f8d698aea36fcbead2b9d5359ffca76f'; // md5 checksum of the string 'Smarty'
    var $_version              =   '1.5.1';    // Smarty version number
    var $_extract              =   false;      // flag for custom functions
    var $_inclusion_depth      =   0;          // current template inclusion depth
    var $_compile_id           =   null;       // for different compiled templates
    var $_smarty_debug_id      =   'SMARTY_DEBUG'; // text in URL to enable debug mode
    var $_smarty_debug_info    =   array();    // debugging information for debug console
    var $_cache_info           =   array();    // info that makes up a cache file


/*======================================================================*\
    Function: Smarty
    Purpose:  Constructor
\*======================================================================*/
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


/*======================================================================*\
    Function:   assign()
    Purpose:    assigns values to template variables
\*======================================================================*/
    function assign($tpl_var, $value = NULL)
    {
        if (is_array($tpl_var)){
            foreach ($tpl_var as $key => $val) {
                if (!empty($key) && isset($val)) {
                    $this->_tpl_vars[$key] = $val;
                }
            }
        } else {
            if (!empty($tpl_var) && isset($value))
                $this->_tpl_vars[$tpl_var] = $value;
        }
        $this->_extract = true;
    }


/*======================================================================*\
    Function: append
    Purpose:  appens values to template variables
\*======================================================================*/
    function append($tpl_var, $value = NULL)
    {
        if (is_array($tpl_var)) {
            foreach ($tpl_var as $key => $val) {
                if (!empty($key)) {
                    if (!is_array($this->_tpl_vars[$key]))
                        settype($this->_tpl_vars[$key], 'array');
                    $this->_tpl_vars[$key][] = $val;
                }
            }
        } else {
            if (!empty($tpl_var) && isset($value)) {
                if (!is_array($this->_tpl_vars[$tpl_var]))
                    settype($this->_tpl_vars[$tpl_var], 'array');
                $this->_tpl_vars[$tpl_var][] = $value;
            }
        }
        $this->_extract = true;
    }


/*======================================================================*\
    Function:   clear_assign()
    Purpose:    clear the given assigned template variable.
\*======================================================================*/
    function clear_assign($tpl_var)
    {
        if (is_array($tpl_var))
            foreach ($tpl_var as $curr_var)
                unset($this->_tpl_vars[$curr_var]);
        else
            unset($this->_tpl_vars[$tpl_var]);
    }


/*======================================================================*\
    Function: register_function
    Purpose:  Registers custom function to be used in templates
\*======================================================================*/
    function register_function($function, $function_impl)
    {
        $this->custom_funcs[$function] = $function_impl;
    }

/*======================================================================*\
    Function: unregister_function
    Purpose:  Unregisters custom function
\*======================================================================*/
    function unregister_function($function)
    {
        unset($this->custom_funcs[$function]);
    }

/*======================================================================*\
    Function: register_compiler_function
    Purpose:  Registers compiler function
\*======================================================================*/
    function register_compiler_function($function, $function_impl)
    {
        $this->compiler_funcs[$function] = $function_impl;
    }

/*======================================================================*\
    Function: unregister_compiler_function
    Purpose:  Unregisters compiler function
\*======================================================================*/
    function unregister_compiler_function($function)
    {
        unset($this->compiler_funcs[$function]);
    }

/*======================================================================*\
    Function: register_modifier
    Purpose:  Registers modifier to be used in templates
\*======================================================================*/
    function register_modifier($modifier, $modifier_impl)
    {
        $this->custom_mods[$modifier] = $modifier_impl;
    }

/*======================================================================*\
    Function: unregister_modifier
    Purpose:  Unregisters modifier
\*======================================================================*/
    function unregister_modifier($modifier)
    {
        unset($this->custom_mods[$modifier]);
    }

/*======================================================================*\
    Function: register_resource
    Purpose:  Registers a resource to fetch a template
\*======================================================================*/
    function register_resource($name, $function_name)
    {
        $this->resource_funcs[$name] = $function_name;
    }

/*======================================================================*\
    Function: unregister_resource
    Purpose:  Unregisters a resource
\*======================================================================*/
    function unregister_resource($name)
    {
        unset($this->resource_funcs[$name]);
    }

/*======================================================================*\
    Function: register_prefilter
    Purpose:  Registers a prefilter function to apply
              to a template before compiling
\*======================================================================*/
    function register_prefilter($function_name)
    {
        $this->prefilter_funcs[] = $function_name;
    }

/*======================================================================*\
    Function: unregister_prefilter
    Purpose:  Unregisters a prefilter function
\*======================================================================*/
    function unregister_prefilter($function_name)
    {
        $tmp_array = array();
        foreach($this->prefilter_funcs as $curr_func) {
            if ($curr_func != $function_name) {
                $tmp_array[] = $curr_func;
            }
        }
        $this->prefilter_funcs = $tmp_array;
    }

/*======================================================================*\
    Function: register_postfilter
    Purpose:  Registers a postfilter function to apply
              to a compiled template after compilation
\*======================================================================*/
    function register_postfilter($function_name)
    {
        $this->postfilter_funcs[] = $function_name;
    }

/*======================================================================*\
    Function: unregister_postfilter
    Purpose:  Unregisters a postfilter function
\*======================================================================*/
    function unregister_postfilter($function_name)
    {
        $tmp_array = array();
        foreach($this->postfilter_funcs as $curr_func) {
            if ($curr_func != $function_name) {
                $tmp_array[] = $curr_func;
            }
        }
        $this->postfilter_funcs = $tmp_array;
    }

/*======================================================================*\
    Function:   clear_cache()
    Purpose:    clear cached content for the given template and cache id
\*======================================================================*/
    function clear_cache($tpl_file = null, $cache_id = null, $compile_id = null)
    {
        if (!isset($compile_id))
            $compile_id = $this->compile_id;

        if (isset($compile_id) || isset($cache_id))
            $auto_id = $compile_id . $cache_id;
        else
            $auto_id = null;
        
        if (!empty($this->cache_handler_func)) {
			$funcname = $this->cache_handler_func;
            return $funcname('clear', $this, $dummy, $tpl_file, $cache_id, $compile_id);
        } else {
            return $this->_rm_auto($this->cache_dir, $tpl_file, $auto_id);
        }
    }


/*======================================================================*\
    Function:   clear_all_cache()
    Purpose:    clear the entire contents of cache (all templates)
\*======================================================================*/
    function clear_all_cache()
    {
        if (!empty($this->cache_handler_func)) {
			$funcname = $this->cache_handler_func;
            return $funcname('clear', $this, $dummy);
        } else {
            return $this->_rm_auto($this->cache_dir);
        }
    }


/*======================================================================*\
    Function:   is_cached()
    Purpose:    test to see if valid cache exists for this template
\*======================================================================*/
    function is_cached($tpl_file, $cache_id = null, $compile_id = null)
    {
        if (!$this->caching)
            return false;

        if (!isset($compile_id))
            $compile_id = $this->compile_id;
        return $this->_read_cache_file($tpl_file, $cache_id, $compile_id, $results);
    }


/*======================================================================*\
    Function:   clear_all_assign()
    Purpose:    clear all the assigned template variables.
\*======================================================================*/
    function clear_all_assign()
    {
        $this->_tpl_vars = array();
    }

/*======================================================================*\
    Function:   clear_compiled_tpl()
    Purpose:    clears compiled version of specified template resource,
                or all compiled template files if one is not specified.
                This function is for advanced use only, not normally needed.
\*======================================================================*/
    function clear_compiled_tpl($tpl_file = null, $compile_id = null)
    {
        if (!isset($compile_id))
            $compile_id = $this->compile_id;
        return $this->_rm_auto($this->compile_dir, $tpl_file, $compile_id);
    }

/*======================================================================*\
    Function: get_template_vars
    Purpose:  Returns an array containing template variables
\*======================================================================*/
    function &get_template_vars()
    {
        return $this->_tpl_vars;
    }


/*======================================================================*\
    Function:   display()
    Purpose:    executes & displays the template results
\*======================================================================*/
    function display($tpl_file, $cache_id = null, $compile_id = null)
    {
        $this->fetch($tpl_file, $cache_id, $compile_id, true);
    }

/*======================================================================*\
    Function:   fetch()
    Purpose:    executes & returns or displays the template results
\*======================================================================*/
    function fetch($_smarty_tpl_file, $_smarty_cache_id = null, $_smarty_compile_id = null, $_smarty_display = false)
    {
        global $HTTP_SERVER_VARS, $QUERY_STRING, $HTTP_COOKIE_VARS;

        if (!$this->debugging && $this->debugging_ctrl == 'URL'
                && (!empty($QUERY_STRING) && strstr($QUERY_STRING, $this->_smarty_debug_id))) {
            $this->debugging = true;
        }

        if ($this->debugging) {
            // capture time for debugging info
            $debug_start_time = $this->_get_microtime();
            $this->_smarty_debug_info[] = array('type'      => 'template',
                                                'filename'  => $_smarty_tpl_file,
                                                'depth'     => 0);
            $included_tpls_idx = count($this->_smarty_debug_info) - 1;
        }

        if (!isset($_smarty_compile_id))
            $_smarty_compile_id = $this->compile_id;

        $this->_inclusion_depth = 0;

        if ($this->caching) {

            $this->_cache_info[] = array('template', $_smarty_tpl_file);

            if ($this->_read_cache_file($_smarty_tpl_file, $_smarty_cache_id, $_smarty_compile_id, $_smarty_results)) {
                if ($this->insert_tag_check) {
                    $_smarty_results = $this->_process_cached_inserts($_smarty_results);
                }
                if ($_smarty_display) {
                    echo $_smarty_results;
                    if ($this->debugging)
                    {
                        // capture time for debugging info
                        $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = $this->_get_microtime() - $debug_start_time;

                        echo $this->_generate_debug_output();
                    }
                    return;
                } else {
                    return $_smarty_results;
                }
            }
        }

        $this->_assign_smarty_interface();

        if ($this->_conf_obj === null) {
            /* Prepare the configuration object. */
            if (!class_exists('Config_File'))
                include_once SMARTY_DIR.'Config_File.class.php';
            $this->_conf_obj = new Config_File($this->config_dir);
            $this->_conf_obj->read_hidden = false;
        } else
            $this->_conf_obj->set_path($this->config_dir);

        extract($this->_tpl_vars);

        /* Initialize config array. */
        $this->_config = array(array('vars'  => array(),
                                     'files' => array()));

        if ($this->show_info_header) {
            $_smarty_info_header = '<!-- Smarty '.$this->_version.' '.strftime("%Y-%m-%d %H:%M:%S %Z").' -->'."\n\n";
        } else {
            $_smarty_info_header = '';
        }

        $compile_path = $this->_get_compile_path($_smarty_tpl_file);

		
		$_smarty_trusted = false;
		if ($this->security) {
			$this->_parse_file_path($this->template_dir, $_smarty_tpl_file, $resource_type, $resource_name);
			if ($this->_is_trusted($resource_type, $resource_name)) {
			$_smarty_trusted = true;
			$this->security = false;
			}
		}
        // if we just need to display the results, don't perform output
        // buffering - for speed
        if ($_smarty_display && !$this->caching) {
            echo $_smarty_info_header;
            if ($this->_process_template($_smarty_tpl_file, $compile_path))
            {
                if ($this->show_info_include) {
                    echo "\n<!-- SMARTY_BEGIN: ".$_smarty_tpl_file." -->\n";
                }
                include($compile_path);
                if ($this->show_info_include) {
                    echo "\n<!-- SMARTY_END: ".$_smarty_tpl_file." -->\n";
                }
            }
        } else {
            ob_start();
            echo $_smarty_info_header;
            if ($this->_process_template($_smarty_tpl_file, $compile_path))
            {
                if ($this->show_info_include) {
                    echo "\n<!-- SMARTY_BEGIN: ".$_smarty_tpl_file." -->\n";
                }
                	include($compile_path);
                if ($this->show_info_include) {
                    echo "\n<!-- SMARTY_END: ".$_smarty_tpl_file." -->\n";
                }
            }
            $_smarty_results = ob_get_contents();
            ob_end_clean();
        }
		if ($_smarty_trusted) {
			$this->security = true;
		}

        if ($this->caching) {
            $this->_write_cache_file($_smarty_tpl_file, $_smarty_cache_id, $_smarty_compile_id, $_smarty_results);
            $_smarty_results = $this->_process_cached_inserts($_smarty_results);
        }

        if ($_smarty_display) {
            if (isset($_smarty_results)) { echo $_smarty_results; }
            if ($this->debugging)
                {
                    // capture time for debugging info
                    $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = ($this->_get_microtime() - $debug_start_time);

                    echo $this->_generate_debug_output();
                }
            return;
        } else {
            if (isset($_smarty_results)) { return $_smarty_results; }
        }
    }


/*======================================================================*\
    Function: _assign_smarty_interface
    Purpose:  assign $smarty interface variable
\*======================================================================*/
    function _assign_smarty_interface()
    {
        $egpcs  = array('e'        => 'env',
                        'g'        => 'get',
                        'p'        => 'post',
                        'c'        => 'cookies',
                        's'        => 'server');
        $globals_map = array('get'      => 'HTTP_GET_VARS',
                             'post'     => 'HTTP_POST_VARS',
                             'cookies'  => 'HTTP_COOKIE_VARS',
                             'session'  => 'HTTP_SESSION_VARS',
                             'server'   => 'HTTP_SERVER_VARS',
                             'env'      => 'HTTP_ENV_VARS');

        $smarty  = array('request'  => array());

        foreach ($globals_map as $key => $array) {
            $smarty[$key] = isset($GLOBALS[$array]) ? $GLOBALS[$array] : array();
        }

        foreach (preg_split('!!', strtolower($this->request_vars_order)) as $c) {
            if (isset($egpcs[$c])) {
                $smarty['request'] = array_merge($smarty['request'], $smarty[$egpcs[$c]]);
            }
        }
        $smarty['request'] = @array_merge($smarty['request'], $smarty['session']);

        $this->_smarty_vars = $smarty;
    }


/*======================================================================*\
    Function:   _generate_debug_output()
    Purpose:    generate debug output
\*======================================================================*/

function _generate_debug_output() {
    // we must force compile the debug template in case the environment
    // changed between separate applications.
    ob_start();
    $force_compile_orig = $this->force_compile;
    $this->force_compile = true;
    $compile_path = $this->_get_compile_path($this->debug_tpl);
    if ($this->_process_template($this->debug_tpl, $compile_path))
    {
        if ($this->show_info_include) {
          echo "\n<!-- SMARTY_BEGIN: ".$this->debug_tpl." -->\n";
        }
        include($compile_path);
        if ($this->show_info_include) {
          echo "\n<!-- SMARTY_END: ".$this->debug_tpl." -->\n";
        }
    }
    $results = ob_get_contents();
    $this->force_compile = $force_compile_orig;
    ob_end_clean();
    return $results;
}

/*======================================================================*\
    Function:   _is_trusted()
    Purpose:	determines if a template is within the trusted_dir or not.
\*======================================================================*/
function _is_trusted($resource_type, $resource_name)
{
	$_smarty_trusted = false;
	if (!empty($this->trusted_dir)) {
		// see if template file is within a trusted directory. If so,
		// disable security during the execution of the template.

		if ($resource_type == 'file') {
			if (!empty($this->trusted_dir)) {
				foreach ((array)$this->trusted_dir as $curr_dir) {
					if ( !empty($curr_dir) && is_readable ($curr_dir)) {
                		if (substr(realpath($resource_name),0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
                    		$_smarty_trusted = true;
                    		break;
						}
					}
                }				
			}
		} else {
			// resource is not on local file system
			$_smarty_trusted = false;
		}
	}
	return $_smarty_trusted;
}		

/*======================================================================*\
    Function:   _is_secure()
    Purpose:	determins if a template is secure or not.
\*======================================================================*/
	function _is_secure($resource_type, $resource_name) {
	
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
		$_smarty_secure = true;
	}
	
	return $_smarty_secure;
}		


/*======================================================================*\
    Function:   _process_template()
    Purpose:
\*======================================================================*/
    function _process_template($tpl_file, $compile_path)
    {		
		// test if template needs to be compiled
        if (!$this->force_compile && $this->_compiled_template_exists($compile_path)) {
            if (!$this->compile_check) {
                // no need to check if the template needs recompiled
                return true;
            } else {
                // get template source and timestamp
                if (!$this->_fetch_template_info($tpl_file, $template_source,
                                                 $template_timestamp)) {
                    return false;
                }
                if ($template_timestamp <= $this->_fetch_compiled_template_timestamp($compile_path)) {
                    // template not expired, no recompile
                    return true;
                } else {
                    // compile template
                    $this->_compile_template($tpl_file, $template_source, $template_compiled);
                    $this->_write_compiled_template($compile_path, $template_compiled);
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
            $this->_write_compiled_template($compile_path, $template_compiled);
            return true;
        }
    }

/*======================================================================*\
    Function:   _get_compile_path
    Purpose:    Get the compile path for this template file
\*======================================================================*/
    function _get_compile_path($tpl_file)
    {
        return $this->_get_auto_filename($this->compile_dir, $tpl_file,
                                         $this->_compile_id);
    }


/*======================================================================*\
    Function:   _compiled_template_exists
    Purpose:
\*======================================================================*/
    function _compiled_template_exists($include_path)
    {
        // everything is in $compile_dir
        return file_exists($include_path);
    }

/*======================================================================*\
    Function:   _fetch_compiled_template_timestamp
    Purpose:
\*======================================================================*/
    function _fetch_compiled_template_timestamp($include_path)
    {
        // everything is in $compile_dir
        return filemtime($include_path);
    }

/*======================================================================*\
    Function:   _write_compiled_template
    Purpose:
\*======================================================================*/
    function _write_compiled_template($compile_path, $template_compiled)
    {
        // we save everything into $compile_dir
        $this->_write_file($compile_path, $template_compiled, true);
        return true;
    }

/*======================================================================*\
    Function:   _parse_file_path
    Purpose:	parse out the type and name from the template resource
\*======================================================================*/
	function _parse_file_path($file_base_path, $file_path, &$resource_type, &$resource_name) {
	
        // split tpl_path by the first colon
        $file_path_parts = explode(':', $file_path, 2);

        if (count($file_path_parts) == 1) {
            // no resource type, treat as type "file"
            $resource_type = 'file';
            $resource_name = $file_path_parts[0];
        } else {
            $resource_type = $file_path_parts[0];
            $resource_name = $file_path_parts[1];
        }
		
		if ($resource_type == 'file') {		
        	if (!preg_match("/^([\/\\\\]|[a-zA-Z]:[\/\\\\])/", $resource_name)) {
            	// relative pathname to $file_base_path
				// use the first directory where the file is found
				foreach((array)$file_base_path as $curr_path) {
					if(@is_file($curr_path.'/'.$resource_name)) {
            			$resource_name = $curr_path.'/'.$resource_name;
						return true;
					}
				}
				// didn't find the file
				return false;
        	}
		}
		
		// resource type != file
		return true;
	}	
	
	
/*======================================================================*\
    Function:   _fetch_template_info()
    Purpose:    fetch the template info. Gets timestamp, and source
                if get_source is true
\*======================================================================*/
    function _fetch_template_info($tpl_path, &$template_source, &$template_timestamp, $get_source=true)
    {
        $_return = false;
		if($this->_parse_file_path($this->template_dir, $tpl_path, $resource_type, $resource_name)) {
        	switch ($resource_type) {
            	case 'file':				
                	if (@is_file($resource_name)) {
                    	if ($get_source) {
                        	$template_source = $this->_read_file($resource_name);
                    	}
                    	$template_timestamp = filemtime($resource_name);
						$_return = true;
                	}
                	break;
            	default:
                	if (isset($this->resource_funcs[$resource_type])) {
                    	$funcname = $this->resource_funcs[$resource_type];
                    	if (function_exists($funcname)) {
                        	// call the function to fetch the template
                        	$_return = $funcname($resource_name, $template_source, $template_timestamp, $get_source, $this);
                    	}
                	}
                	break;
        	}		
		}
		if(!$_return) {
			// see if we can get a template with the default template handler
			if(!empty($this->default_template_handler_func)) {
				if(!function_exists($this->default_template_handler_func)) {
                    $this->_trigger_error_msg("default template handler function \"$this->default_template_handler_func\" doesn't exist.");
                    $_return = false;
				}
				$funcname = $this->default_template_handler_func;
				$_return = $funcname($resource_type, $resource_name, $template_source, $template_timestamp, $this);
			}			
		}
		
		if(!$_return) {
			$this->_trigger_error_msg("unable to read template resource: \"$tpl_path\"");			
		} elseif ($_return && $this->security && !$this->_is_secure($resource_type, $resource_name) && !$this->_is_trusted($resource_type, $resource_name)) {
            $this->_trigger_error_msg("(secure mode) accessing \"$tpl_path\" is not allowed");
			$template_source = null;
			$template_timestamp = null;
			return false;
        }
        return $_return;
    }


/*======================================================================*\
    Function:   _compile_template()
    Purpose:    called to compile the templates
\*======================================================================*/
    function _compile_template($tpl_file, $template_source, &$template_compiled)
    {
        include_once SMARTY_DIR.$this->compiler_class . '.class.php';

        $smarty_compiler = new $this->compiler_class;

        $smarty_compiler->template_dir      = $this->template_dir;
        $smarty_compiler->compile_dir       = $this->compile_dir;
        $smarty_compiler->config_dir        = $this->config_dir;
        $smarty_compiler->force_compile     = $this->force_compile;
        $smarty_compiler->caching           = $this->caching;
        $smarty_compiler->php_handling      = $this->php_handling;
        $smarty_compiler->left_delimiter    = $this->left_delimiter;
        $smarty_compiler->right_delimiter   = $this->right_delimiter;
        $smarty_compiler->custom_funcs      = $this->custom_funcs;
        $smarty_compiler->custom_mods       = $this->custom_mods;
        $smarty_compiler->_version          = $this->_version;
        $smarty_compiler->prefilter_funcs   = $this->prefilter_funcs;
        $smarty_compiler->postfilter_funcs  = $this->postfilter_funcs;
        $smarty_compiler->compiler_funcs    = $this->compiler_funcs;
        $smarty_compiler->security          = $this->security;
        $smarty_compiler->secure_dir        = $this->secure_dir;
        $smarty_compiler->security_settings = $this->security_settings;
        $smarty_compiler->trusted_dir       = $this->trusted_dir;

        if ($smarty_compiler->_compile_file($tpl_file, $template_source, $template_compiled))
            return true;
        else {
            $this->_trigger_error_msg($smarty_compiler->_error_msg);
            return false;
        }
    }

/*======================================================================*\
    Function:   _smarty_include()
    Purpose:    called for included templates
\*======================================================================*/
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
        extract($this->_tpl_vars);

        array_unshift($this->_config, $this->_config[0]);
        $compile_path = $this->_get_compile_path($_smarty_include_tpl_file);

		$this->_parse_file_path($this->template_dir, $_smarty_include_tpl_file, $resource_type, $resource_name);
		if ($this->security && $this->_is_trusted($resource_type, $resource_name)) {
			$_smarty_trusted = true;
			$this->security = false;
		} else {
			$_smarty_trusted = false;
		}
		
        if ($this->_process_template($_smarty_include_tpl_file, $compile_path)) {
            if ($this->show_info_include) {
                echo "\n<!-- SMARTY_BEGIN: ".$_smarty_include_tpl_file." -->\n";
            }
            include($compile_path);
            if ($this->show_info_include) {
                echo "\n<!-- SMARTY_END: ".$_smarty_include_tpl_file." -->\n";
            }
        }
		
		if ($_smarty_trusted) {
			$this->security = true;
		}

        array_shift($this->_config);
        $this->_inclusion_depth--;

        if ($this->debugging) {
            // capture time for debugging info
            $this->_smarty_debug_info[$included_tpls_idx]['exec_time'] = $this->_get_microtime() - $debug_start_time;
        }

        if ($this->caching) {
            $this->_cache_info[] = array('template', $_smarty_include_tpl_file);
        }
    }

/*======================================================================*\
    Function: _config_load
    Purpose:  load configuration values
\*======================================================================*/
    function _config_load($file, $section, $scope)
    {
        if ($this->debugging) {
            $debug_start_time = $this->_get_microtime();
        }

        if ($this->caching) {
            $this->_cache_info[] = array('config', $file);
        }

        if (!isset($this->_config[0]['files'][$file])) {
            $this->_config[0]['vars'] = array_merge($this->_config[0]['vars'], $this->_conf_obj->get($file));
            $this->_config[0]['files'][$file] = true;
        }
        if ($scope == 'parent') {
            if (count($this->_config) > 0 && !isset($this->_config[1]['files'][$file])) {
                $this->_config[1]['vars'] = array_merge($this->_config[1]['vars'], $this->_conf_obj->get($file));
                $this->_config[1]['files'][$file] = true;
            }
        } else if ($scope == 'global')
            for ($i = 1; $i < count($this->_config); $i++) {
                if (!isset($this->_config[$i]['files'][$file])) {
                    $this->_config[$i]['vars'] = array_merge($this->_config[$i]['vars'], $this->_conf_obj->get($file));
                    $this->_config[$i]['files'][$file] = true;
                }
            }

        if (!empty($section)) {
            $this->_config[0]['vars'] = array_merge($this->_config[0]['vars'], $this->_conf_obj->get($file, $section));
            if ($scope == 'parent') {
                if (count($this->_config) > 0)
                    $this->_config[1]['vars'] = array_merge($this->_config[1]['vars'], $this->_conf_obj->get($file, $section));
            } else if ($scope == 'global')
                for ($i = 1; $i < count($this->_config); $i++)
                    $this->_config[$i]['vars'] = array_merge($this->_config[$i]['vars'], $this->_conf_obj->get($file, $section));
        }

        if ($this->debugging) {
            $debug_start_time = $this->_get_microtime();
            $this->_smarty_debug_info[] = array('type'      => 'config',
                                                'filename'  => $file.' ['.$section.'] '.$scope,
                                                'depth'     => $this->_inclusion_depth,
                                                'exec_time' => $this->_get_microtime() - $debug_start_time);
        }
    }


/*======================================================================*\
    Function: _process_cached_inserts
    Purpose:  Replace cached inserts with the actual results
\*======================================================================*/
    function _process_cached_inserts($results)
    {

        preg_match_all('!'.$this->_smarty_md5.'{insert_cache (.*)}'.$this->_smarty_md5.'!Uis',
                       $results, $match);
        list($cached_inserts, $insert_args) = $match;

        for ($i = 0; $i < count($cached_inserts); $i++) {
        	if ($this->debugging) {
            	$debug_start_time = $this->_get_microtime();
        	}

            $args = unserialize($insert_args[$i]);

            $name = $args['name'];
            unset($args['name']);

            if (isset($args['script'])) {
                $this->_parse_file_path($this->trusted_dir, $this->_dequote($args['script']), $resource_type, $resource_name);
                if ($this->security) {
                    if( $resource_type != 'file' || !@is_file($resource_name)) {
                        $this->_syntax_error("include_php: $resource_type: $resource_name is not readable");                return false;
                    }
                    if (!$this->_is_trusted($resource_type, $resource_name)) {
                        $this->_syntax_error("include_php: $resource_type: $resource_name is not trusted");
                        return false;
                    }
                }
                include_once($resource_name);
                unset($args['script']);
            }

            $function_name = 'insert_' . $name;
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


/*======================================================================*\
    Function: _run_insert_handler
    Purpose:  Handle insert tags
\*======================================================================*/
function _run_insert_handler($args)
{
    if ($this->debugging) {
        $debug_start_time = $this->_get_microtime();
    }

    if ($this->caching) {
        $arg_string = serialize($args);
        return $this->_smarty_md5."{insert_cache $arg_string}".$this->_smarty_md5;
    } else {
        $function_name = 'insert_'.$args['name'];
        if (isset($args['script'])) {
            $this->_parse_file_path($this->trusted_dir, $this->_dequote($args['script']), $resource_type, $resource_name);
            if ($this->security) {
                if( $resource_type != 'file' || !@is_file($resource_name)) {
                    $this->_syntax_error("include_php: $resource_type: $resource_name is not readable");                return false;
                }
                if (!$this->_is_trusted($resource_type, $resource_name)) {
                    $this->_syntax_error("include_php: $resource_type: $resource_name is not trusted");
                    return false;
                }
            }
            include_once($resource_name);
        }

        $content = $function_name($args, $this);
        if ($this->debugging) {
            $this->_smarty_debug_info[] = array('type'      => 'insert',
                                                'filename'  => 'insert_'.$args['name'],
                                                'depth'     => $this->_inclusion_depth,
                                                'exec_time' => $this->_get_microtime() - $debug_start_time);
        }
        if (!empty($args["assign"])) {
            $this->assign($args["assign"],$content);
        } else {
            return $content;
        }
    }
}


/*======================================================================*\
    Function: _run_mod_handler
    Purpose:  Handle modifiers
\*======================================================================*/
    function _run_mod_handler()
    {
        $args = func_get_args();
        list($func_name, $map_array) = array_splice($args, 0, 2);
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


/*======================================================================*\
    Function: _dequote
    Purpose:  Remove starting and ending quotes from the string
\*======================================================================*/
    function _dequote($string)
    {
        if (($string{0} == "'" || $string{0} == '"') &&
            $string{strlen($string)-1} == $string{0})
            return substr($string, 1, -1);
        else
            return $string;
    }


/*======================================================================*\
    Function:   _read_file()
    Purpose:    read in a file from line $start for $lines.
                read the entire file if $start and $lines are null.
\*======================================================================*/
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

/*======================================================================*\
    Function:   _write_file()
    Purpose:    write out a file
\*======================================================================*/
    function _write_file($filename, $contents, $create_dirs = false)
    {
        if ($create_dirs)
            $this->_create_dir_structure(dirname($filename));

        if (!($fd = @fopen($filename, 'w'))) {
            $this->_trigger_error_msg("problem writing '$filename.'");
            return false;
        }

        // flock doesn't seem to work on several windows platforms (98, NT4, NT5, ?),
        // so we'll not use it at all in windows.

        if ( strtoupper(substr(PHP_OS, 0, 3)) == 'WIN' || (flock($fd, LOCK_EX)) ) {
            fwrite( $fd, $contents );
            fclose($fd);
            chmod($filename, 0644);
        }

        return true;
    }

/*======================================================================*\
    Function: _get_auto_base
    Purpose:  Get a base name for automatic files creation
\*======================================================================*/
    function _get_auto_base($auto_base, $auto_source)
    {
        $source_md5 = md5($auto_source);

        $res = $auto_base . '/' . substr($source_md5, 0, 2) . '/' . $source_md5;

        return $res;
    }

/*======================================================================*\
    Function: _get_auto_filename
    Purpose:  get a concrete filename for automagically created content
\*======================================================================*/
    function _get_auto_filename($auto_base, $auto_source, $auto_id = null)
    {
        $res = $this->_get_auto_base($auto_base, $auto_source) .
                '/' . md5($auto_id) . '.php';

        return $res;
    }

/*======================================================================*\
    Function: _rm_auto
    Purpose: delete an automagically created file by name and id
\*======================================================================*/
    function _rm_auto($auto_base, $auto_source = null, $auto_id = null)
    {
        if (!is_dir($auto_base))
          return false;

        if (!isset($auto_source)) {
            $res = $this->_rmdir($auto_base, 0);
        } else {
            if (isset($auto_id)) {
                $tname = $this->_get_auto_filename($auto_base, $auto_source, $auto_id);
                $res = is_file($tname) && unlink( $tname);
            } else {
                $tname = $this->_get_auto_base($auto_base, $auto_source);
                $res = $this->_rmdir($tname);
            }
        }

        return $res;
    }

/*======================================================================*\
    Function: _rmdir
    Purpose: delete a dir recursively (level=0 -> keep root)
    WARNING: no security whatsoever!!
\*======================================================================*/
    function _rmdir($dirname, $level = 1)
    {
        $handle = opendir($dirname);

        while ($entry = readdir($handle)) {
            if ($entry != '.' && $entry != '..') {
                if (is_dir($dirname . '/' . $entry)) {
                    $this->_rmdir($dirname . '/' . $entry, $level + 1);
                }
                else {
                    unlink($dirname . '/' . $entry);
                }
            }
        }

        closedir($handle);

        if ($level)
            @rmdir($dirname);

        return true;
    }

/*======================================================================*\
    Function: _create_dir_structure
    Purpose:  create full directory structure
\*======================================================================*/
    function _create_dir_structure($dir)
    {
        if (!file_exists($dir)) {
            $dir_parts = preg_split('!/+!', $dir, -1, PREG_SPLIT_NO_EMPTY);
            $new_dir = ($dir{0} == '/') ? '/' : '';
            foreach ($dir_parts as $dir_part) {
                $new_dir .= $dir_part;
                if (!file_exists($new_dir) && !mkdir($new_dir, 0771)) {
                    $this->_trigger_error_msg("problem creating directory \"$dir\"");
                    return false;
                }
                $new_dir .= '/';
            }
        }
    }

/*======================================================================*\
    Function:   _write_cache_file
    Purpose:    Prepend the cache information to the cache file
                and write it
\*======================================================================*/
    function _write_cache_file($tpl_file, $cache_id, $compile_id, $results)
    {
        // put timestamp in cache header
        $this->_cache_info['timestamp'] = time();

        // prepend the cache header info into cache file
        $results = 'SMARTY_CACHE_INFO_HEADER'.serialize($this->_cache_info)."\n".$results;

        if (!empty($this->cache_handler_func)) {
            // use cache_handler function
			$funcname = $this->cache_handler_func;
            return $funcname('write', $this, $results, $tpl_file, $cache_id, $compile_id);
        } else {    
            // use local cache file
            if (isset($compile_id) || isset($cache_id))
                $auto_id = $compile_id . $cache_id;
            else
                $auto_id = null;

            $cache_file = $this->_get_auto_filename($this->cache_dir, $tpl_file, $auto_id);
            $this->_write_file($cache_file, $results, true);
            return true;
        }
    }

/*======================================================================*\
    Function:   _read_cache_file
    Purpose:    read a cache file, determine if it needs to be
                regenerated or not
\*======================================================================*/
    function _read_cache_file($tpl_file, $cache_id, $compile_id, &$results)
    {
        if ($this->force_compile || $this->cache_lifetime == 0) {
            // force compile enabled or cache lifetime is zero, always regenerate
            return false;
        }

        if (!empty($this->cache_handler_func)) {

            // use cache_handler function
			$funcname = $this->cache_handler_func;
            $funcname('read', $this, $results, $tpl_file, $cache_id, $compile_id);

        } else {
            // use local file cache
            if (isset($compile_id) || isset($cache_id))
                $auto_id = $compile_id . $cache_id;
            else
                $auto_id = null;

            $cache_file = $this->_get_auto_filename($this->cache_dir, $tpl_file, $auto_id);
            $results = $this->_read_file($cache_file);

        }
		
		if (empty($results)) {
			// nothing to parse (error?), regenerate cache
			return false;
		}
				
        $cache_split = explode("\n", $results, 2);
        $cache_header = $cache_split[0];

        if (substr($cache_header, 0, 24) == 'SMARTY_CACHE_INFO_HEADER') {
            $cache_info = unserialize(substr($cache_header, 24));
            $cache_timestamp = $cache_info['timestamp'];

            if (time() - $cache_timestamp > $this->cache_lifetime) {
                // cache expired, regenerate
                return false;
            }

            if ($this->compile_check) {
                foreach ($cache_info as $curr_cache_info) {
                    switch ($curr_cache_info[0]) {
                        case 'template':
                            $this->_fetch_template_info($curr_cache_info[1], $template_source, $template_timestamp, false);
                            if ($cache_timestamp < $template_timestamp) {
                                // template file has changed, regenerate cache
                                return false;
                            }
                            break;

                        case 'config':
                            if ($cache_timestamp < filemtime($this->config_dir.'/'.$curr_cache_info[1])) {
                                // config file file has changed, regenerate cache
                                return false;
                            }
                            break;
                    }
                }
            }
            $results = $cache_split[1];
            return true;
        } else {
            // no cache info header, regenerate cache
            return false;
        }
    }


/*======================================================================*\
    Function:   quote_replace
    Purpose:    Quote subpattern references
\*======================================================================*/
    function quote_replace($string)
    {
        return preg_replace('![\\$]\d!', '\\\\\\0', $string);
    }


/*======================================================================*\
    Function: _trigger_error_msg
    Purpose:  trigger Smarty error
\*======================================================================*/
    function _trigger_error_msg($error_msg, $error_type = E_USER_WARNING)
    {
        trigger_error("Smarty error: $error_msg", $error_type);
    }

/*======================================================================*\
    Function:   _get_microtime
    Purpose:    Get seconds and microseconds
\*======================================================================*/
    function _get_microtime()
    {
        $mtime = microtime();
        $mtime = explode(" ", $mtime);
        $mtime = (double)($mtime[1]) + (double)($mtime[0]);
        return ($mtime);
    }

}

/* vim: set expandtab: */

?>

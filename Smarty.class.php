<?php
/*
 * Project:     Smarty: the PHP compiling template engine
 * File:        Smarty.class.php
 * Author:      Monte Ohrt <monte@ispi.net>
 *              Andrei Zmievski <andrei@ispi.net>
 *
 * Version:             1.3.1
 * Copyright:           2001 ispi of Lincoln, Inc.
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
 * You may contact the authors of Smarty by e-mail at:
 * monte@ispi.net
 * andrei@ispi.net
 *
 * Or, write to:
 * Monte Ohrt
 * CTO, ispi
 * 237 S. 70th suite 220
 * Lincoln, NE 68510
 *
 * The latest version of Smarty can be obtained from:
 * http://www.phpinsider.com/
 *
 */

require('Smarty.addons.php');

define("SMARTY_PHP_PASSTHRU",0);
define("SMARTY_PHP_QUOTE",1);
define("SMARTY_PHP_REMOVE",2);
define("SMARTY_PHP_ALLOW",3);

class Smarty
{

    // public vars
    var $template_dir    =  './templates';     // name of directory for templates  
    var $compile_dir     =  './templates_c';   // name of directory for compiled templates 
    var $config_dir      =  './configs';       // directory where config files are located

    var $global_assign   =  array('SCRIPT_NAME'); // variables from the GLOBALS array
                                                  // that are implicitly assigned
                                                  // to all templates   
    var $compile_check   =  true;       // whether to check for compiling step or not:
                                        // This is generally set to false once the
                                        // application is entered into production and
                                        // initially compiled. Leave set to true
                                        // during development. true/false default true.

    var $force_compile   =  false;      // force templates to compile every time.
                                        // if cache file exists, it will
                                        // override compile_check and force_compile.
                                        // true/false. default false.
    var $caching         =  false;      // whether to use caching or not. true/false
    var $cache_dir       =  './cache';  // name of directory for template cache
    var $cache_lifetime  =  3600;       // number of seconds cached content will persist.
                                        // 0 = never expires. default is one hour (3600)

    var $tpl_file_ext    =  '.tpl';     // template file extention
    
    var $php_handling    =  SMARTY_PHP_PASSTHRU; // how smarty handles php tags
                                        // possible values:
                                        // SMARTY_PHP_PASSTHRU -> echo tags as is
                                        // SMARTY_PHP_QUOTE    -> escape tags as entities
                                        // SMARTY_PHP_REMOVE   -> remove php tags
                                        // SMARTY_PHP_ALLOW    -> execute php tags
                                        // default: SMARTY_PHP_PASSTHRU

    var $left_delimiter  =  '{';        // template tag delimiters.
    var $right_delimiter =  '}';
    
    var $custom_funcs    =  array(  'html_options'      => 'smarty_func_html_options',
                                    'html_select_date'  => 'smarty_func_html_select_date',
                                    'math'              => 'smarty_func_math',
                                    'fetch'             => 'smarty_func_fetch'
                                 );
    
    var $custom_mods     =  array(  'lower'         => 'strtolower',
                                    'upper'         => 'strtoupper',
                                    'capitalize'    => 'ucwords',
                                    'escape'        => 'smarty_mod_escape',
                                    'truncate'      => 'smarty_mod_truncate',
                                    'spacify'       => 'smarty_mod_spacify',
                                    'date_format'   => 'smarty_mod_date_format',
                                    'string_format' => 'smarty_mod_string_format',
                                    'replace'       => 'smarty_mod_replace',
                                    'strip_tags'    => 'smarty_mod_strip_tags',
                                    'default'       => 'smarty_mod_default'
                                 );

    // internal vars
    var $_error_msg             =   false;      // error messages. true/false
    var $_tpl_vars              =   array();
    var $_smarty_md5            =   'f8d698aea36fcbead2b9d5359ffca76f'; // md5 checksum of the string 'Smarty'    
    
/*======================================================================*\
    Function: Smarty
    Purpose:  Constructor
\*======================================================================*/
    function Smarty()
    {
        foreach ($this->global_assign as $var_name) {
            if(isset($GLOBALS[$var_name])) {
                $this->assign($var_name, $GLOBALS[$var_name]);
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
                if (!empty($key))
                    $this->_tpl_vars[$key] = $val;
            }
        } else {
            if (!empty($tpl_var) && isset($value))
                $this->_tpl_vars[$tpl_var] = $value;
        }
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
    }


/*======================================================================*\
    Function:   clear_assign()
    Purpose:    clear the given assigned template variable.
\*======================================================================*/
    function clear_assign($tpl_var)
    {
        if(is_array($tpl_var))
            foreach($tpl_var as $curr_var)
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
    Function:   clear_cache()
    Purpose:    clear cached content for the given template and cache id
\*======================================================================*/
    function clear_cache($tpl_file, $cache_id = null)
    {
        $cache_tpl_md5 = md5(realpath($this->template_dir.'/'.$tpl_file));
        $cache_dir = $this->cache_dir.'/'.$cache_tpl_md5;
        
        if (!is_dir($cache_dir))
            return false;

        if (isset($cache_id)) {
            $cache_id_md5 = md5($cache_id);
            $cache_id_dir = substr($cache_id_md5, 0, 2);
            $cache_file = "$cache_dir/$cache_id_dir/{$cache_tpl_md5}_$cache_id_md5.cache";
            return (bool)(is_file($cache_file) && unlink($cache_file));
        } else 
            return $this->_clear_tpl_cache_dir($cache_tpl_md5);
    }
    
    
/*======================================================================*\
    Function:   clear_all_cache()
    Purpose:    clear the entire contents of cache (all templates)
\*======================================================================*/
    function clear_all_cache()
    {
        if (!is_dir($this->cache_dir))
            return false;

        $dir_handle = opendir($this->cache_dir);
        while ($curr_dir = readdir($dir_handle)) {
            if ($curr_dir == '.' || $curr_dir == '..' ||
                !is_dir($this->cache_dir.'/'.$curr_dir))
                continue;

            $this->_clear_tpl_cache_dir($curr_dir);
        }
        closedir($dir_handle);

        return true;
    }


/*======================================================================*\
    Function:   is_cached()
    Purpose:    test to see if valid cache exists for this template
\*======================================================================*/
    function is_cached($tpl_file, $cache_id = null)
    {
        if (!$this->caching)
            return false;

        // cache name = template path + cache_id
        $cache_tpl_md5 = md5(realpath($this->template_dir.'/'.$tpl_file));
        $cache_id_md5 = md5($cache_id);
        $cache_id_dir = substr($cache_id_md5, 0, 2);
        $cache_file = $this->cache_dir."/$cache_tpl_md5/$cache_id_dir/{$cache_tpl_md5}_$cache_id_md5.cache";

        if (file_exists($cache_file) &&
            ($this->cache_lifetime == 0 ||
             (time() - filemtime($cache_file) <= $this->cache_lifetime)))
            return true;
        else
            return false;
        
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
    function display($tpl_file, $cache_id = null)
    {
        $this->fetch($tpl_file, $cache_id, true);
    }   
        
/*======================================================================*\
    Function:   fetch()
    Purpose:    executes & returns or displays the template results
\*======================================================================*/
    function fetch($tpl_file, $cache_id = null, $display = false)
    {
        global $HTTP_SERVER_VARS;

        if ($this->caching) {
            // cache name = template path + cache_id
            $cache_tpl_md5 = md5(realpath($this->template_dir.'/'.$tpl_file));
            $cache_id_md5 = md5($cache_id);
            $cache_id_dir = substr($cache_id_md5, 0, 2);
            $cache_file = $this->cache_dir."/$cache_tpl_md5/$cache_id_dir/{$cache_tpl_md5}_$cache_id_md5.cache";
            
            if (file_exists($cache_file) &&
                ($this->cache_lifetime == 0 ||
                 (time() - filemtime($cache_file) <= $this->cache_lifetime))) {
                $results = $this->_read_file($cache_file);
                $results = $this->_process_cached_inserts($results);
                if ($display) {
                    echo $results;
                    return;
                } else
                    return $results;
            }
        }

        // compile files
        $this->_compile($this->template_dir);
        //assemble compile directory path to file
        $_compile_file = $this->compile_dir."/".$tpl_file.".php";

        extract($this->_tpl_vars);

        // if we just need to display the results, don't perform output
        // buffering - for speed
        if ($display && !$this->caching)
            include($_compile_file);
        else {
            ob_start();
            include($_compile_file);
            $results = ob_get_contents();
            ob_end_clean();
        }

        if($this->caching) {
            $this->_write_file($cache_file, $results, true);
            $results = $this->_process_cached_inserts($results);
        }

        if ($display) {
            if(isset($results)) { echo $results; }
            return;
        } else {
            if(isset($results)) { return $results; }
        }
    }   
    
/*======================================================================*\
    Function:   compile()
    Purpose:    called to compile the templates
\*======================================================================*/
    function _compile($tpl_dir)
    {
        if($this->compile_check || $this->force_compile)
        {
            include_once("Smarty_Compiler.class.php");
            
            $smarty_compile = new Smarty_Compiler;

            $smarty_compile->template_dir = $this->template_dir;            
            $smarty_compile->compile_dir = $this->compile_dir;            
            $smarty_compile->config_dir = $this->config_dir;            
            $smarty_compile->force_compile = $this->force_compile;            
            $smarty_compile->caching = $this->caching;            
            $smarty_compile->tpl_file_ext = $this->tpl_file_ext;            
            $smarty_compile->php_handling = $this->php_handling;            
            $smarty_compile->left_delimiter = $this->left_delimiter;  
            $smarty_compile->right_delimiter = $this->right_delimiter;            
            $smarty_compile->custom_funcs = $this->custom_funcs;            
            $smarty_compile->custom_mods = $this->custom_mods;           
            
            if($smarty_compile->_traverse_files($tpl_dir, 0))
                return true;
            else {
                $this->_error_msg = $smarty_compile->_error_msg;
                return false;
            }
        } else
            return false;
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

            $args = unserialize($insert_args[$i]);

            $name = $args['name'];
            unset($args['name']);

            $function_name = 'insert_' . $name;
            $replace = $function_name($args);

            $results = str_replace($cached_inserts[$i], $replace, $results);
        }

        return $results;
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
    Purpose:    read in a file
\*======================================================================*/
    function _read_file($filename)

    {
        if (!($fd = fopen($filename, 'r'))) {
            $this->_set_error_msg("problem reading '$filename.'");
            return false;
        }
        flock($fd, LOCK_SH);
        $contents = fread($fd, filesize($filename));
        fclose($fd);
        return $contents;
    }

/*======================================================================*\
    Function:   _write_file()
    Purpose:    write out a file
\*======================================================================*/
    function _write_file($filename, $contents, $create_dirs = false)
    {
        if($create_dirs)
            $this->_create_dir_structure(dirname($filename));
        
        if (!($fd = fopen($filename, 'a'))) {
            $this->_set_error_msg("problem writing '$filename.'");
            return false;
        }
        flock($fd, LOCK_EX);

        $fd_safe = fopen($filename, 'w');

        fwrite($fd_safe, $contents);
        fclose($fd_safe);
        fclose($fd);

        return true;
    }    


/*======================================================================*\
    Function: _clear_tpl_cache_dir
    Purpose:  Clear the specified template cache
\*======================================================================*/
    function _clear_tpl_cache_dir($cache_tpl_md5)
    {
        $cache_dir = $this->cache_dir.'/'.$cache_tpl_md5;

        if (!is_dir($cache_dir))
            return false;

        $dir_handle = opendir($cache_dir);
        while ($curr_dir = readdir($dir_handle)) {
            $cache_path_dir = $cache_dir.'/'.$curr_dir;
            if ($curr_dir == '.' || $curr_dir == '..' ||
                !is_dir($cache_path_dir))
                continue;

            $dir_handle2 = opendir($cache_path_dir);
            while ($curr_file = readdir($dir_handle2)) {
                if ($curr_file == '.' || $curr_file == '..')
                    continue;

                $cache_file = $cache_path_dir.'/'.$curr_file;
                if (is_file($cache_file))
                    unlink($cache_file);
            }
            closedir($dir_handle2);
            @rmdir($cache_path_dir);
        }
        closedir($dir_handle);
        @rmdir($cache_dir);

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
                if (!file_exists($new_dir) && !mkdir($new_dir, 0755)) {
                    $this->_set_error_msg("problem creating directory \"$dir\"");
                    return false;               
                }
                $new_dir .= '/';
            }
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
    Function:   _set_error_msg()
    Purpose:    set the error message
\*======================================================================*/
    function _set_error_msg($error_msg)
    {
        $this->_error_msg="smarty error: $error_msg";
        return true;
    }

/*======================================================================*\
    Function: _syntax_error
    Purpose:  display Smarty syntax error
\*======================================================================*/
    function _syntax_error($error_msg, $error_type = E_USER_ERROR)
    {
        trigger_error("Smarty: [in " . $this->_current_file . " line " .
                      $this->_current_line_no . "]: syntax error: $error_msg", $error_type);
    }
}

/* vim: set expandtab: */

?>

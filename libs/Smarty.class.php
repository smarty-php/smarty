<?
/*
 * Project:		Smarty: the PHP compiling template engine
 * File:		Smarty.class.php
 * Author:		Monte Ohrt <monte@ispi.net>
 *              Andrei Zmievski <andrei@ispi.net>
 *
 * Version:             1.2.3
 * Copyright:           2001 ispi of Lincoln, Inc.
 *				
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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

require("Smarty.addons.php");

class Smarty
{

	// public vars
	var	$compile_check	 =	true;       // whether to check for compiling step or not:
                                        // This is generally set to false once the
                                        // application is entered into production and
                                        // initially compiled. Leave set to true
                                        // during development. true/false

	var $force_compile	 =	false;      // force templates to compile every time.
                                        // overrides compile_check. true/false
	
                                        // NOTE: all cache directives override
                                        // compiling directives. If a cached version
                                        // is available, that will be used regardless
                                        // of compile settings.
	var $caching	     =	true;       // whether to use caching or not. true/false
	var $cache_lifetime	 =	3600;       // number of seconds cached content will persist.
                                        // 0 = never expires. default is one hour (3600)

	var $template_dir	 =	"./templates";      // name of directory for templates	
	var $compile_dir	 =	"./templates_c";    // name of directory for compiled templates	
	var $cache_dir		 =	"./cache";          // name of directory for template cache
	
	
	var $tpl_file_ext	 =	".tpl";	    // template file extentions
	
	var $allow_php		 =	false;      // whether or not to allow embedded php
                                        // in the templates. By default, php tags
                                        // are escaped. true/false

	var $left_delimiter	 =	"{";        // template tag delimiters.
	var $right_delimiter =	"}";

	var $config_dir		 =	"./configs";	// directory where config files are located

	var $custom_funcs	 =	array(	'html_options'		=> 'smarty_func_html_options',
									'html_select_date'	=> 'smarty_func_html_select_date'
								 );
	
	var $custom_mods	 =	array(	'lower'			=> 'strtolower',
									'upper'			=> 'strtoupper',
									'capitalize'	=> 'ucwords',
									'escape'		=> 'smarty_mod_escape',
									'truncate'		=> 'smarty_mod_truncate',
									'spacify'		=> 'smarty_mod_spacify',
									'date_format'	=> 'smarty_mod_date_format',
									'string_format'	=> 'smarty_mod_string_format',
									'replace'		=> 'smarty_mod_replace',
									'strip_tags'	=> 'smarty_mod_strip_tags',
									'default'		=> 'smarty_mod_default'
								 );
	var $global_assign	 =	array(	'SCRIPT_NAME'
								 );
	
	// internal vars
	var $_error_msg				=	false;		// error messages. true/false
	var $_tpl_vars				= 	array();
	var $_sectionelse_stack		=	array();	// keeps track of whether section had 'else' part
	var $_literal_blocks		=	array();	// keeps literal template blocks
	var $_current_file			=	null;		// the current template being compiled
	var $_current_line_no		=	1;			// line number for error messages

	
/*======================================================================*\
	Function: Smarty
	Purpose:  Constructor
\*======================================================================*/
	function Smarty()
	{
		foreach ($this->global_assign as $var_name)
			$this->assign($var_name, $GLOBALS[$var_name]);
	}


/*======================================================================*\
	Function:	assign()
	Purpose:	assigns values to template variables
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
	Function:	clear_assign()
	Purpose:	clear the given assigned template variable.
\*======================================================================*/

	function clear_assign($tpl_var)
	{
		unset($this->_tpl_vars[$tpl_var]);
	}

/*======================================================================*\
	Function:	clear_all_assign()
	Purpose:	clear all the assigned template variables.
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
	Function:	display()
	Purpose:	executes & displays the template results
\*======================================================================*/

	function display($tpl_file)
	{
		$this->fetch($tpl_file, true);
	}	
		
/*======================================================================*\
	Function:	fetch()
	Purpose:	executes & returns or displays the template results
\*======================================================================*/

	function fetch($tpl_file, $display = false)
	{
		global $PHP_SELF;

		if($this->caching) {
			// cache id = template path + the invoked script
			$cache_file = $this->cache_dir."/".urlencode($tpl_file."@".$PHP_SELF).".cache";
 			if(!$this->cache_force &&
					(file_exists($cache_file) &&
						($this->cache_lifetime == 0 ||
							(mktime() - filemtime($cache_file) <= $this->cache_lifetime)
						))) {
					$results = $this->_read_file($cache_file);
					$results = $this->_process_cached_inserts($results);
                    if ($display) {
                        print $results;
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
        // buferring - for speed
        if ($display && !$this->caching)
            include($_compile_file);
        else {
            ob_start();
            include($_compile_file);
            $results = ob_get_contents();
            ob_end_clean();
        }

		if($this->caching) {
			$this->_write_file($cache_file, $results);
			$results = $this->_process_cached_inserts($results);
		}

        if ($display) {
            print $results;
            return;
        } else
            return $results;
	}

/*======================================================================*\
	Function:	clear_all_cache()
	Purpose:	clear all the cached template files.
\*======================================================================*/

	function clear_all_cache()
	{
		while($curr_file = readdir($this->cache_dir)) {
			if ($curr_file == '.' || $curr_file == '..')
				continue;
			if(substr($curr_file,-6) == '.cache')
				unlink($curr_file);
		}		
	}
	
	
/*======================================================================*\
	Function:	compile()
	Purpose:	called to compile the templates
\*======================================================================*/

	function _compile($tpl_dir)
	{
		if($this->compile_check || $this->force_compile)
		{
			if($this->_traverse_files($tpl_dir, 0))
				return true;
			else
				return false;
		}
		else
			return false;
	}	
	
/*======================================================================*\
	Function:	_traverse_files()
	Purpose:	traverse the template files & process each one
\*======================================================================*/

	function _traverse_files($tpl_dir, $depth)
	{
		if(is_dir($tpl_dir)) {
			if($tpl_dir)
				$dir_handle = opendir($tpl_dir);

			while($curr_file = readdir($dir_handle)) {
				if ($curr_file == '.' || $curr_file == '..')
					continue;

				$filepath = $tpl_dir."/".$curr_file;
				if(is_readable($filepath)) {
					
					if(is_file($filepath) && substr($curr_file, -strlen($this->tpl_file_ext)) == $this->tpl_file_ext) {
						if(!$this->_process_file($filepath))
							return false;
					} else if (is_dir($filepath)) {
						if (!$this->_traverse_files($filepath, $depth + 1))
							return false;
					} else {
						// invalid file type, skipping
						$this->_set_error_msg("Invalid filetype for $filepath, skipping");
						continue;
					}
				}
			}
		} else {
			$this->_set_error_msg("Directory \"$tpl_dir\" does not exist or is not a directory.");
			return false;
		}

		return true;
	}

/*======================================================================*\
	Function:	_process_file()
	Input:		test template files for modifications
				and execute the compilation for each
				one requiring it.
\*======================================================================*/

	function _process_file($filepath)
	{
		if(preg_match("/^(.+)\/([^\/]+)$/", $filepath, $match)) {
			$tpl_file_dir = $match[1];			
			$tpl_file_name = $match[2] . ".php";

			$compile_dir = preg_replace('!^' . preg_quote($this->template_dir, '!') . '!',
										$this->compile_dir, $match[1]);

			//create directory if none exists
			if(!file_exists($compile_dir)) {
				$compile_dir_parts = preg_split('!/+!', $compile_dir);
				$new_dir = "";
				foreach ($compile_dir_parts as $dir_part) {
					$new_dir .= $dir_part."/";
					if (!file_exists($new_dir) && !mkdir($new_dir, 0755)) {
						$this->_set_error_msg("problem creating directory \"$compile_dir\"");
						return false;				
					}
				}
			}

			// compile the template file if none exists or has been modified or recompile is forced
			if ($this->force_compile || !file_exists($compile_dir."/".$tpl_file_name) ||
				($this->_modified_file($filepath, $compile_dir."/".$tpl_file_name))) {
				if (!$this->_compile_file($filepath, $compile_dir."/".$tpl_file_name))
					return false;
			} else {
				// no compilation needed
				return true;
			}
		} else {
			$this->_set_error_msg("problem matching \"$filepath.\"");
			return false;
		}

		return true;
	}

/*======================================================================*\
	Function:	_modified_file()
	Input:		return comparison of modification times of files
\*======================================================================*/

	function _modified_file($filepath, $compilepath)
	{
		if(filemtime($filepath) >= filemtime($compilepath))
			return true;
		return false;
	}

/*======================================================================*\
	Function:	_compile_file()
	Input:		compile a template file
\*======================================================================*/

	function _compile_file($filepath, $compilepath)
	{
		if (!($template_contents = $this->_read_file($filepath)))
			return false;

		$this->_current_file = str_replace($this->template_dir . "/", "", $filepath);
		$this->_current_line_no = 1;
		$ldq = preg_quote($this->left_delimiter, '!');
		$rdq = preg_quote($this->right_delimiter, '!');

		/* Pull out the literal blocks. */
		preg_match_all("!{$ldq}literal{$rdq}(.*?){$ldq}/literal{$rdq}!s", $template_contents, $match);
		$this->_literal_blocks = $match[1];
		$template_contents = preg_replace("!{$ldq}literal{$rdq}(.*?){$ldq}/literal{$rdq}!s",
						 				  '{literal}', $template_contents);

		/* Gather all template tags. */
		preg_match_all("!{$ldq}\s*(.*?)\s*{$rdq}!s", $template_contents, $match);
		$template_tags = $match[1];
		/* Split content by template tags to obtain non-template content. */
		$text_blocks = preg_split("!{$ldq}.*?{$rdq}!s", $template_contents);
		if(!$this->allow_php) {
			/* Escape php tags. */
			$text_blocks = preg_replace('!<\?([^?]*?)\?>!', '&lt;?$1?&gt;', $text_blocks);
		}

		/* Compile the template tags into PHP code. */
		$compiled_tags = array();
		for ($i = 0; $i < count($template_tags); $i++) {
			$this->_current_line_no += substr_count($text_blocks[$i], "\n");
			$compiled_tags[] = $this->_compile_tag($template_tags[$i]);
			$this->_current_line_no += substr_count($template_tags[$i], "\n");
		}

		$compiled_contents = "";
		
		/* Interleave the compiled contents and text blocks to get the final result. */
		for ($i = 0; $i < count($compiled_tags); $i++) {
			$compiled_contents .= $text_blocks[$i].$compiled_tags[$i];
		}
		$compiled_contents .= $text_blocks[$i];
		
		/* Reformat data between 'strip' and '/strip' tags, removing spaces, tabs and newlines. */
		if (preg_match_all("!{$ldq}strip{$rdq}.*?{$ldq}/strip{$rdq}!s", $compiled_contents, $match)) {
			$strip_tags = $match[0];
			$strip_tags_modified = preg_replace("!{$ldq}/?strip{$rdq}|[\t ]+$|^[\t ]+!m", '', $strip_tags);
			$strip_tags_modified = preg_replace('![\r\n]+!m', '', $strip_tags_modified);
			for ($i = 0; $i < count($strip_tags); $i++)
				$compiled_contents = preg_replace("!{$ldq}strip{$rdq}.*?{$ldq}/strip{$rdq}!s",
												  $strip_tags_modified[$i], $compiled_contents, 1);
		}

		if(!$this->_write_file($compilepath, $compiled_contents))
			return false;

		return true;
	}

	function _process_cached_inserts($results)
	{
		preg_match_all('!\{\{\{insert_cache (.*)\}\}\}!Uis', $results, $match);
		
		list($cached_inserts, $insert_args) = $match;

		for ($i = 0; $i < count($cached_inserts); $i++) {
			$attrs = $this->_parse_attrs($insert_args[$i], false);
			$name = $this->_dequote($attrs['name']);

			if (empty($name)) {
				$this->_syntax_error("missing insert name");
			}

			$arg_list = array();
			foreach ($attrs as $arg_name => $arg_value) {
				if ($arg_name == 'name') continue;
				$arg_list[$arg_name] = $arg_value;
			}

			$function_name = 'insert_' . $name;
			$replace = $function_name($arg_list);
			
			$results = str_replace($cached_inserts[$i], $replace, $results);
		}

		return $results;
	}	
	
	function _compile_tag($template_tag)
	{
		/* Matched comment. */
		if ($template_tag{0} == '*' && $template_tag{strlen($template_tag)-1} == '*')
			return "";

		$qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';

		/* Split tag into two parts: command and the arguments. */
		preg_match('/^(
					   (?: ' . $qstr_regexp . ' | (?>[^"\'\s]+))+
					  )
				      (?:\s+(.*))?
					/xs', $template_tag, $match);
		list(, $tag_command, $tag_args) = $match;

		/* If the tag name matches a variable or section property definition,
		   we simply process it. */
		if (preg_match('!^\$(\w+/)*\w+(?>\.\w+)*(?>\|@?\w+(:(' . $qstr_regexp . '|[^|]+))?)*$!', $tag_command) ||	// if a variable
			preg_match('!^#(\w+)#(?>\|@?\w+(:(' . $qstr_regexp . '|[^|]+))?)*$!', $tag_command)		||  // or a configuration variable
			preg_match('!^%\w+\.\w+%(?>\|@?\w+(:(' . $qstr_regexp . '|[^|]+))?)*$!', $tag_command)) {    // or a section property
			settype($tag_command, 'array');
			$this->_parse_vars_props($tag_command);
			return "<?php print $tag_command[0]; ?>";
		}

		switch ($tag_command) {
			case 'include':
				return $this->_compile_include_tag($tag_args);

			case 'if':
				return $this->_compile_if_tag($tag_args);

			case 'else':
				return '<?php else: ?>';

			case 'elseif':
				return $this->_compile_if_tag($tag_args, true);

			case '/if':
				return '<?php endif; ?>';

			case 'ldelim':
				return $this->left_delimiter;

			case 'rdelim':
				return $this->right_delimiter;

			case 'section':
				array_push($this->_sectionelse_stack, false);
				return $this->_compile_section_start($tag_args);

			case 'sectionelse':
				$this->_sectionelse_stack[count($this->_sectionelse_stack)-1] = true;
				return "<?php endfor; else: ?>";

			case '/section':
				if (array_pop($this->_sectionelse_stack))
					return "<?php endif; ?>";
				else
					return "<?php endfor; endif; ?>";

			case 'config_load':
				return $this->_compile_config_load_tag($tag_args);

			case 'strip':
			case '/strip':
				return $this->left_delimiter.$tag_command.$this->right_delimiter;

			case 'literal':
				list (,$literal_block) = each($this->_literal_blocks);
				$this->_current_line_no += substr_count($literal_block, "\n");
				return $literal_block;

			case 'insert':
				return $this->_compile_insert_tag($tag_args);

			default:
				if (isset($this->custom_funcs[$tag_command])) {
					return $this->_compile_custom_tag($tag_command, $tag_args);
				} else {
					$this->_syntax_error("unknown tag - '$tag_command'", E_USER_WARNING);
					return "";
				}
		}
	}

	function _compile_custom_tag($tag_command, $tag_args)
	{
		$attrs = $this->_parse_attrs($tag_args);
		$function = $this->custom_funcs[$tag_command];
		foreach ($attrs as $arg_name => $arg_value) {
			if (is_bool($arg_value))
				$arg_value = $arg_value ? 'true' : 'false';
			$arg_list[] = "'$arg_name' => $arg_value";
		}

		return "<?php $function(array(".implode(',', (array)$arg_list).")); ?>";
	}

	function _compile_insert_tag($tag_args)
	{
		$attrs = $this->_parse_attrs($tag_args);
		$name = substr($attrs['name'], 1, -1);

		if (empty($name)) {
			$this->_syntax_error("missing insert name");
		}

		if($this->caching)
			return "{{{insert_cache $tag_args}}}";
		
		foreach ($attrs as $arg_name => $arg_value) {
			if ($arg_name == 'name') continue;
			if (is_bool($arg_value))
				$arg_value = $arg_value ? 'true' : 'false';
			$arg_list[] = "'$arg_name' => $arg_value";
		}

		return "<?php print insert_$name(array(".implode(',', (array)$arg_list).")); ?>";
	}	
	
	function _compile_config_load_tag($tag_args)
	{
		$attrs = $this->_parse_attrs($tag_args);

		if (empty($attrs['file'])) {
			$this->_syntax_error("missing 'file' attribute in config_load tag");
		}

		$output  = "<?php if (!class_exists('Config_File'))\n" .
				   "	include_once 'Config_File.class.php';\n" .
	    		   "if (!is_object(\$_conf_obj) || get_class(\$_conf_obj) != 'config_file') {\n" .
				   "	\$_conf_obj  = new Config_File('".$this->config_dir."');\n" .
	    		   "}\n" .
	   			   "\$_config = array_merge((array)\$_config, \$_conf_obj->get(".$attrs['file']."));\n";

		if (!empty($attrs['section']))
			$output .= '$_config = array_merge((array)$_config, $_conf_obj->get('.$attrs['file'].', '.$attrs['section'].')); ';

		$output .= '?>';

		return $output;
	}


	function _compile_include_tag($tag_args)
	{
		$attrs = $this->_parse_attrs($tag_args);

		if (empty($attrs['file'])) {
			$this->_syntax_error("missing 'file' attribute in include tag");
		} else
			$attrs['file'] = $this->_dequote($attrs['file']);

		if (count($attrs) > 1) {
			$include_func_name = uniqid("_include_");
			$include_file_name = $this->compile_dir.'/'.$attrs['file'];

			foreach ($attrs as $arg_name => $arg_value) {
				if ($arg_name == 'file') continue;
				if (is_bool($arg_value))
					$arg_value = $arg_value ? 'true' : 'false';
				$arg_list[] = "'$arg_name' => $arg_value";
			}

			return 	"<?php " .
					"function $include_func_name(\$file_name, \$def_vars, \$include_vars)\n" .
					"{\n" .
					"	extract(\$def_vars);\n" .
					"	extract(\$include_vars);\n" .
					"	include \"\$file_name.php\";\n" .
					"}\n" .
					"$include_func_name(\"$include_file_name\", get_defined_vars(), array(".implode(',', (array)$arg_list)."));\n?>\n";
		} else
			 return '<?php include "'.$this->compile_dir.'/'.$attrs['file'].'.php"; ?>';
	}

	function _compile_section_start($tag_args)
	{
		$attrs = $this->_parse_attrs($tag_args);

		$output = "<?php ";
		$section_name = $attrs['name'];
		if (empty($section_name)) {
			$this->_syntax_error("missing section name");
		}

		$output .= "unset(\$_sections[$section_name]);\n";
		$section_props = "\$_sections[$section_name]['properties']";

		foreach ($attrs as $attr_name => $attr_value) {
			switch ($attr_name) {
				case 'loop':
					$output .= "{$section_props}['loop'] = is_array($attr_value) ? count($attr_value) : $attr_value;\n";
					break;

				case 'show':
					if (is_bool($attr_value))
						$attr_value = $attr_value ? 'true' : 'false';
					$output .= "{$section_props}['$attr_name'] = $attr_value;\n";
					break;

				default:
					$output .= "{$section_props}['$attr_name'] = $attr_value;\n";
					break;
			}
		}

		if (isset($attrs['loop'])) {
			$loop_check_code = "{$section_props}['loop'] > 0 && ";
		} else {
			$output .= "{$section_props}['loop'] = 1;\n";
		}

		if (isset($attrs['show'])) {
			$show_check_code = "{$section_props}['show'] && ";
		} else {
			$output .= "{$section_props}['show'] = {$section_props}['loop'] > 0;\n";
		}

		$output .= "if ($loop_check_code $show_check_code true): ";

		$output .= "
			for ({$section_props}['index'] = 0;
				 {$section_props}['index'] < {$section_props}['loop'];
				 {$section_props}['index']++):\n";
		$output .= "{$section_props}['rownum'] = {$section_props}['index'] + 1;\n";

		$output .= "?>\n";

		return $output;
	}

	function _compile_if_tag($tag_args, $elseif = false)
	{
		/* Tokenize args for 'if' tag. */
		preg_match_all('/(?:
						 "[^"\\\\]*(?:\\\\.[^"\\\\]*)*" 		| # match all double quoted strings allowed escaped double quotes
						 \'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'		| # match all single quoted strings allowed escaped single quotes
						 [()]									| # match parentheses
						 [^"\'\s()]+							  # match any other token that is not any of the above
						)/x', $tag_args, $match);
		$tokens = $match[0];

		$this->_parse_vars_props($tokens);

		$is_arg_stack = array();
		
		for ($i = 0; $i < count($tokens); $i++) {
			$token = &$tokens[$i];
			switch ($token) {
				case 'eq':
					$token = '==';
					break;

				case 'ne':
				case 'neq':
					$token = '!=';
					break;

				case 'lt':
					$token = '<';
					break;

				case 'le':
				case 'lte':
					$token = '<=';
					break;

				case 'gt':
					$token = '>';
					break;
				
				case 'ge':
				case 'gte':
					$token = '>=';
					break;

				case 'and':
					$token = '&&';
					break;

				case 'or':
					$token = '||';
					break;

				case 'not':
					$token = '!';
					break;

				case 'mod':
					$token = '%';
					break;

				case '(':
					array_push($is_arg_stack, $i);
					break;

				case 'is':
					/* If last token was a ')', we operate on the parenthesized
					   expression. The start of the expression is on the stack.
					   Otherwise, we operate on the last encountered token. */
					if ($tokens[$i-1] == ')')
						$is_arg_start = array_pop($is_arg_stack);
					else
						$is_arg_start = $i-1;
					/* Construct the argument for 'is' expression, so it knows
					   what to operate on. */
					$is_arg = implode(' ', array_slice($tokens, $is_arg_start, $i - $is_arg_start));

					/* Pass all tokens from next one until the end to the
					   'is' expression parsing function. The function will
					   return modified tokens, where the first one is the result
					   of the 'is' expression and the rest are the tokens it
					   didn't touch. */
					$new_tokens = $this->_parse_is_expr($is_arg, array_slice($tokens, $i+1));

					/* Replace the old tokens with the new ones. */
					array_splice($tokens, $is_arg_start, count($tokens), $new_tokens);

					/* Adjust argument start so that it won't change from the
					   current position for the next iteration. */
					$i = $is_arg_start;
					break;
			}
		}

		if ($elseif)
			return '<?php elseif ('.implode(' ', $tokens).'): ?>';
		else
			return '<?php if ('.implode(' ', $tokens).'): ?>';
	}

	function _parse_is_expr($is_arg, $tokens)
	{
		$expr_end = 0;

		if (($first_token = array_shift($tokens)) == 'not') {
			$negate_expr = true;
			$expr_type = array_shift($tokens);
		} else
			$expr_type = $first_token;

		switch ($expr_type) {
			case 'even':
				if ($tokens[$expr_end] == 'by') {
					$expr_end++;
					$expr_arg = $tokens[$expr_end++];
					$expr = "!(($is_arg / $expr_arg) % $expr_arg)";
				}
				else
					$expr = "!($is_arg % 2)";
				break;

			case 'odd':
				if ($tokens[$expr_end] == 'by') {
					$expr_end++;
					$expr_arg = $tokens[$expr_end++];
					$expr = "(($is_arg / $expr_arg) % $expr_arg)";
				}
				else
					$expr = "($is_arg % 2)";
				break;

			case 'div':
				if ($tokens[$expr_end] == 'by') {
					$expr_end++;
					$expr_arg = $tokens[$expr_end++];
					$expr = "!($is_arg % $expr_arg)";
				} else {
					$this->_syntax_error("expecting 'by' after 'div'");
				}
				break;

			default:
				$this->_syntax_error("unknown 'is' expression - '$expr_type'");
				break;
		}

		if ($negate_expr) {
			$expr = "!($expr)";
		}

		array_splice($tokens, 0, $expr_end, $expr);

		return $tokens;
	}

	function _parse_attrs($tag_args, $quote = true)
	{
		/* Tokenize tag attributes. */
		preg_match_all('/(?:"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"       | 
						  \'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'     | (?>[^"\'=\s]+)
						 )+ |
						 [=]
					    /x', $tag_args, $match);
		$tokens = $match[0];
		$var_delims = array('$', '#', '%');

		$attrs = array();
		/* Parse state:
		   	0 - expecting attribute name
			1 - expecting '='
			2 - expecting attribute value (not '=') */
		$state = 0;

		foreach ($tokens as $token) {
			switch ($state) {
				case 0:
					/* If the token is a valid identifier, we set attribute name
					   and go to state 1. */
					if (preg_match('!\w+!', $token)) {
						$attr_name = $token;
						$state = 1;
					} else
						$this->_syntax_error("invalid attribute name - '$token'");
					break;

				case 1:
					/* If the token is '=', then we go to state 2. */
					if ($token == '=') {
						$state = 2;
					} else
						$this->_syntax_error("expecting '=' after attribute name");
					break;

				case 2:
					/* If token is not '=', we set the attribute value and go to
					   state 0. */
					if ($token != '=') {
						/* We booleanize the token if it's a non-quoted possible
						   boolean value. */
						if (preg_match('!^(on|yes|true)$!', $token))
							$token = true;
						else if (preg_match('!^(off|no|false)$!', $token))
							$token = false;
						/* If the token is not variable (doesn't start with
						   '$', '#', or '%') and not enclosed in single or
						   double quotes we single-quote it. */
						else if ($quote && !in_array($token{0}, $var_delims) &&
								 !(($token{0} == '"' || $token[0] == "'") &&
								 $token{strlen($token)-1} == $token{0}))
							$token = "'".$token."'";

						$attrs[$attr_name] = $token;
						$state = 0;
					} else
						$this->_syntax_error("'=' cannot be an attribute value");
					break;
			}
		}

		$this->_parse_vars_props($attrs);

		return $attrs;
	}

	function _preg_grep($pattern, $array)
	{
		$result = array();

		foreach ($array as $key => $entry) {
			if (preg_match($pattern, $entry))
				$result[$key] = $entry;
		}

		return $result;
	}
	
	function _parse_vars_props(&$tokens)
	{
		$qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';

		/* preg_grep() was fixed to return keys properly in 4.0.4 and later. To
		   allow people to use older versions of PHP we emulate preg_grep() and
		   use the version check to see what function to call. */
		if (strnatcmp(PHP_VERSION, '4.0.4') >= 0) {
			$var_exprs = preg_grep('!^\$(\w+/)*\w+(?>\.\w+)*(?>\|@?\w+(:(' .  $qstr_regexp .  '|[^|]+))?)*$!', $tokens);
			$conf_var_exprs = preg_grep('!^#(\w+)#(?>\|@?\w+(:(' . $qstr_regexp . '|[^|]+))?)*$!', $tokens);
			$sect_prop_exprs = preg_grep('!^%\w+\.\w+%(?>\|@?\w+(:(' .  $qstr_regexp .  '|[^|]+))?)*$!', $tokens);
		} else {
			$var_exprs = $this->_preg_grep('!^\$(\w+/)*\w+(?>\.\w+)*(?>\|@?\w+(:(' .  $qstr_regexp .  '|[^|]+))?)*$!', $tokens);
			$conf_var_exprs = $this->_preg_grep('!^#(\w+)#(?>\|@?\w+(:(' . $qstr_regexp . '|[^|]+))?)*$!', $tokens);
			$sect_prop_exprs = $this->_preg_grep('!^%\w+\.\w+%(?>\|@?\w+(:(' .  $qstr_regexp .  '|[^|]+))?)*$!', $tokens);
		}

		if (count($var_exprs)) {
			foreach ($var_exprs as $expr_index => $var_expr) {
				$tokens[$expr_index] = $this->_parse_var($var_expr);
			}
		}
		
		if (count($conf_var_exprs)) {
			foreach ($conf_var_exprs as $expr_index => $var_expr) {
				$tokens[$expr_index] = $this->_parse_conf_var($var_expr);
			}
		}

		if (count($sect_prop_exprs)) {
			foreach ($sect_prop_exprs as $expr_index => $section_prop_expr) {
				$tokens[$expr_index] = $this->_parse_section_prop($section_prop_expr);
			}
		}
	}

	function _parse_var($var_expr)
	{
		list($var_ref, $modifiers) = explode('|', substr($var_expr, 1), 2);

		$sections = explode('/', $var_ref);
		$props = explode('.', array_pop($sections));
		$var_name = array_shift($props);

		$output = "\$$var_name";

		foreach ($sections as $section) {
			$output .= "[\$_sections['$section']['properties']['index']]";
		}
		foreach ($props as $prop) {
			$output .= "['$prop']";
		}

		$this->_parse_modifiers($output, $modifiers);

		return $output;
	}

	function _parse_conf_var($conf_var_expr)
	{
		list($var_ref, $modifiers) = explode('|', $conf_var_expr, 2);

		$var_name = substr($var_ref, 1, -1);

		$output = "\$_config['$var_name']";

		$this->_parse_modifiers($output, $modifiers);

		return $output;
	}

	function _parse_section_prop($section_prop_expr)
	{
		list($var_ref, $modifiers) = explode('|', $section_prop_expr, 2);

		preg_match('!%(\w+)\.(\w+)%!', $var_ref, $match);
		$section_name = $match[1];
		$prop_name = $match[2];

		$output = "\$_sections['$section_name']['properties']['$prop_name']";

		$this->_parse_modifiers($output, $modifiers);

		return $output;
	}

	function _parse_modifiers(&$output, $modifier_string)
	{
		$qstr_regexp = '"[^"\\\\]*(?:\\\\.[^"\\\\]*)*"|\'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'';
		preg_match_all('!\|@?(\w+)((?::(?:'. $qstr_regexp . '|[^|]+))*)!', '|' . $modifier_string, $match);
		list(, $modifiers, $modifier_arg_strings) = $match;

		for ($i = 0; $i < count($modifiers); $i++) {
			$modifier_name = $modifiers[$i];
			preg_match_all('!:(' . $qstr_regexp . '|[^|]+)!', $modifier_arg_strings[$i], $match);
			$modifier_args = $match[1];

			if ($modifier_name{0} == '@') {
				$map_array = 'false';
				$modifier_name = substr($modifier_name, 1);
			} else
				$map_array = 'true';

			/*
			 * First we lookup the modifier function name in the registered
			 * modifiers table.
			 */
			$mod_func_name = $this->custom_mods[$modifier_name];

			/*
			 * If we don't find that modifier there, we assume it's just a PHP
			 * function name.
			 */
			if (!isset($mod_func_name))
				$mod_func_name = $modifier_name;

			$this->_parse_vars_props($modifier_args);

			if (count($modifier_args) > 0)
				$modifier_args = ', '.implode(', ', $modifier_args);
			else
				$modifier_args = '';

			$output = "_smarty_mod_handler('$mod_func_name', $map_array, $output$modifier_args)";
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
	Function:	_read_file()
	Purpose:	read in a file
\*======================================================================*/

	function _read_file($filename)
	{
		if(!($fd = fopen($filename, 'r'))) {
			$this->_set_error_msg("problem reading '$filename.'");
			return false;
		}
		$contents = fread($fd, filesize($filename));
		fclose($fd);
		return $contents;
	}

/*======================================================================*\
	Function:	_write_file()
	Purpose:	write out a file
\*======================================================================*/

	function _write_file($filename,$contents)
	{
		if(!($fd = fopen($filename, 'w'))) {
			$this->_set_error_msg("problem writing '$filename.'");
			return false;
		}
		fwrite($fd, $contents);
		fclose($fd);
		return true;
	}

/*======================================================================*\
	Function:	_set_error_msg()
	Purpose:	set the error message
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

?>

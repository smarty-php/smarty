<?
/*
 * Project:		Smarty: the PHP compiled template engine
 * File:		Smarty.class.php
 * Author:		Monte Ohrt <monte@ispi.net>
 *				original idea and implementation
 *
 *				Andrei Zmievski <andrei@ispi.net>
 *				parser rewrite and optimizations
 *
 */

require("Smarty.addons.php");

class Smarty
{

	// public vars
	var	$compile_check	=	true;	// whether to check for compiling step or not:
									// This is generally set to false once the
									// application is entered into production and
									// initially compiled. Leave set to true
									// during development.

	var $template_dir			=	"templates"; // name of directory for templates	
	
	var $compile_dir_ext		=	"_c";	// the directory extention where
											// compiled templates are placed
	
	var $tpl_file_ext			=	".tpl";	// template file extentions
	
	var $max_recursion_depth	=	10;			// maximum recursion depth.
												// this is to help catch infinite loops.
												// 0 == unlimited recursion.
	var $allow_php				=	false;		// whether or not to allow embedded php
												// in the templates. By default, php tags
												// are escaped.
	var $left_delimiter			=	"{";		// template tag delimiters.
	var $right_delimiter		=	"}";

	
	// registered template functions
	// NOTE: leave off the "smarty_" prefix on the actual PHP function name
	var $registered_functions	=	array(	"htmlesc",
											"urlesc",
											"default",
											"configload",
											"configclear",
											"configprint",
											"configset"
											 );

	var $_modifiers				=	array(	"lower"		=> "smarty_mod_lower",
											"upper"		=> "smarty_mod_upper"
										 );

	// internal vars
	var $errorMsg				=	false;		// error messages
	var $_tpl_vars				= 	array();

/*======================================================================*\
	Function:	assign()
	Purpose:	assign template variables
\*======================================================================*/

	function assign($tpl_var, $value = NULL)
	{
		if(gettype($tpl_var) == "array")
		{
		   foreach ($tpl_var as $key => $val)
			{
				if (!(empty($key)))
					$this->_tpl_vars["$key"] = $val;
			}
		}
		else
		{
			if ((!(empty($tpl_var))) && (isset($value)))
			{
				$this->_tpl_vars["$tpl_var"] = $value;
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
		return true;
	}

/*======================================================================*\
	Function:	clear_all_assign()
	Purpose:	clear all the assigned template variables.
\*======================================================================*/

	function clear_all_assign()
	{
		return($this->_tpl_vars = array());
	}

/*======================================================================*\
	Function:	spew()
	Purpose:	executes & prints the template results
\*======================================================================*/

	function spew($tpl_file)
	{
		if(preg_match("/^(.+)\/([^\/]+)$/",$tpl_file,$match))
		{
			// compile files
			$this->_compile($match[1]);
			//assemble compile directory path to file
			$compile_file = preg_replace("/([\.\/]*[^\/]+)(.*)/","\\1".preg_quote($this->compile_dir_ext,"/")."\\2",$tpl_file);
			
			extract($this->_tpl_vars);		
			/* TODO remove comments */
			/* include($compile_file); */
		}
	}

/*======================================================================*\
	Function:	fetch()
	Purpose:	executes & returns the template results
\*======================================================================*/

	function fetch($tpl_file)
	{
		ob_start();
		$this->spew($tpl_file);
		$results = ob_get_contents();
		ob_end_clean();
		return $results;
	}

/*======================================================================*\
	Function:	compile()
	Purpose:	called to compile the templates
\*======================================================================*/

	function _compile($tpl_dir)
	{
		if($this->compile_check)
		{
			if($this->_traverse_files($tpl_dir,0))
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

	function _traverse_files($tpl_dir,$depth)
	{
		// exit if recursion depth is met
		if($this->max_recursion_depth != 0 && $depth >= $this->max_recursion_depth)
		{
			$this->_set_errorMsg("recursion depth of $depth reached on $tpl_dir/$curr_file. exiting.");
			return false;
		}
		if(is_dir($tpl_dir))
		{
			if($tpl_dir)

			$dir_handle = opendir($tpl_dir);
			while($curr_file = readdir($dir_handle))
			{
				if(!preg_match("/".preg_quote($this->tpl_file_ext,"/")."$/",$curr_file))
				{
					//echo "skipping $curr_file<br>\n";
					continue;
				}
				
				$filepath = $tpl_dir."/".$curr_file;
				//echo "filepath is $filepath<br>\n";
				if(is_readable($filepath))
				{
					if(is_file($filepath))
					{
						//echo "is file.<br>\n";
						//echo $filepath, $depth<br>\n";
						if(!$this->_process_file($filepath))
							return false;						
					}
					elseif(is_dir($filepath))
					{
						//echo "is directory.<br>\n";
						if(!$this->_traverse_files($filepath,$depth+1))
							return false;
					}
					else
					{
						// invalid file type, skipping
						$this->_set_errorMsg("Invalid filetype for $filepath, skipping");
						continue;
					}
				}
			}
		}
		else
		{
			$this->_set_errorMsg("Directory \"$tpl_dir\" does not exist or is not a directory.");
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
		if(preg_match("/^(.+)\/([^\/]+)$/",$filepath,$match))
		{
			$tpl_file_dir = $match[1];			
			$tpl_file_name = $match[2];

			//assemble compile directory path
			$compile_dir = preg_replace("/([\.\/]*[^\/]+)(.*)/","\\1".preg_quote($this->compile_dir_ext,"/")."\\2",$match[1]);
			
			//echo "compile dir: $compile_dir<br>\n";
			//create directory if none exists
			if(!file_exists($compile_dir))
				if(!mkdir($compile_dir,0755))
				{
					$this->_set_errorMsg("problem creating directory \"$compile_dir\"");
					return false;				
				}
			// compile the template file if none exists or has been modified
			/* TODO remove 1 from test */
			if(!file_exists($compile_dir."/".$tpl_file_name) || 1 ||
				($this->_modified_file($filepath,$compile_dir."/".$tpl_file_name)))
			{
				if(!$this->_compile_file($filepath,$compile_dir."/".$tpl_file_name))
					return false;				
			}
			else
			{
				// no compilation needed
				return true;
			}
		}
		else
		{
			$this->_set_errorMsg("problem matching \"$filepath.\"");
			return false;
		}
		return true;
	}

/*======================================================================*\
	Function:	_modified_file()
	Input:		return comparison of modification times of files
\*======================================================================*/

	function _modified_file($filepath,$compilepath)
	{
		if(filemtime($filepath) >= filemtime($compilepath))
			return true;
		return false;
	}

/*======================================================================*\
	Function:	_compile_file()
	Input:		compile a template file
\*======================================================================*/

	function _compile_file($filepath,$compilepath)
	{
		if(!($template_contents = $this->_read_file($filepath)))
			return false;

		$ldq = preg_quote($this->left_delimiter, "/");
		$rdq = preg_quote($this->right_delimiter, "/");

		/* Gather all template tags. */
		preg_match_all("/$ldq\s*(.*?)\s*$rdq/s", $template_contents, $match);
		$template_tags = $match[1];
		/* Split content by template tags to obtain non-template content. */
		$text_blocks = preg_split("/$ldq.*?$rdq/s", $template_contents);

		$compiled_tags = array();
		foreach ($template_tags as $template_tag) {
			$compiled_tags[] = $this->_compile_tag($template_tag);
		}

		for ($i = 0; $i < count($compiled_tags); $i++) {
			$compiled_contents .= $text_blocks[$i].$compiled_tags[$i];
		}
		$compiled_contents .= $text_blocks[$i];
		
		var_dump($compiled_tags);

		// replace literal delimiter tags
		$compiled_contents = preg_replace('/{ldelim}/', $this->left_delimiter,  $compiled_contents);
		$compiled_contents = preg_replace('/{rdelim}/', $this->right_delimiter, $compiled_contents);

		if(!($this->_write_file($compilepath, $compiled_contents)))
			return false;

		return true;
	}

	function _compile_tag($template_tag)
	{
		/* Tokenize the tag. */
		preg_match_all('/(?:
						 "[^"\\\\]*(?:\\\\.[^"\\\\]*)*" 		| # match all double quoted strings allowed escaped double quotes
						 \'[^\'\\\\]*(?:\\\\.[^\'\\\\]*)*\'		| # match all single quoted strings allowed escaped single quotes
						 [()]									| # match parentheses
						 [^"\'\s()]+							  # match any other token that is not any of the above
						)/x', $template_tag, $match);
		$tokens = $match[0];

		/* Matched comment. */
		if ($tokens[0] == '*' && $tokens[count($tokens)-1] == '*')
			return "";

		/* Parse variables and section properties. */
		if (preg_match('!^\$(\w+/)*\w+(?>\|\w+(:[^|]+)?)*$!', $tokens[0]) ||	// if a variable
			preg_match('!^%\w+\.\w+%(?>\|\w+(:[^|]+)?)*$!', $tokens[0])) {   	// or a section property
			$this->_parse_vars_props($tokens);
			return "<?php print $tokens[0]; ?>";
		}

		switch ($tokens[0]) {
			case 'include':
				/* TODO parse file= attribute */
				return '<?php include "'.$this->template_dir.$this->compile_dir_ext.'/'.$tokens[1].'"; ?>';

			case 'if':
				array_shift($tokens);
				return $this->_compile_if_tag($tokens);

			case 'else':
				return '<?php else: ?>';

			case '/if':
				return '<?php endif; ?>';

			case 'ldelim':
				return $this->left_delimiter;

			case 'rdelim':
				return $this->right_delimiter;

			case 'section':
				break;

			case '/section':
				break;

			default:
				/* TODO capture custom functions here */
				break;
		}
	}

	function _compile_if_tag(&$tokens)
	{
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
					$expr = "(($is_arg / $expr_arg) % $expr_arg)";
				}
				else
					$expr = "!($is_arg % 2)";
				break;

			case 'odd':
				if ($tokens[$expr_end] == 'by') {
					$expr_end++;
					$expr_arg = $tokens[$expr_end++];
					$expr = "!(($is_arg / $expr_arg) % $expr_arg)";
				}
				else
					$expr = "($is_arg % 2)";
				break;

			case 'mod':
				$expr_arg = $tokens[$expr_end++];
				$expr = "!($is_arg % $expr_arg)";
				break;

			default:
				/* TODO strict syntax checking */
				break;
		}

		if ($negate_expr) {
			$expr = "!($expr)";
		}

		array_splice($tokens, 0, $expr_end, $expr);

		return $tokens;
	}
	
	function _parse_vars_props(&$tokens)
	{
		$var_exprs = preg_grep('!^\$(\w+/)*\w+(\|\w+(:[^|]+)?)*$!', $tokens);
		$sect_prop_exprs = preg_grep('!^%\w+\.\w+%(\|\w+(:[^|]+)?)*$!', $tokens);

		if (count($var_exprs)) {
			foreach ($var_exprs as $expr_index => $var_expr) {
				$tokens[$expr_index] = $this->_parse_var($var_expr);
			}
		}
		if (count($sect_prop_exprs)) {
			foreach ($section_prop_exprs as $expr_index => $section_prop_expr) {
				$tokens[$expr_index] = $this->_parse_section_prop($section_prop_expr);
			}
		}
	}

	function _parse_var($var_expr)
	{
		$modifiers = explode('|', substr($var_expr, 1));

		$sections = explode('/', array_shift($modifiers));
		$var_name = array_pop($sections);

		$output = "\$$var_name";

		foreach ($sections as $section) {
			$output .= "[\$_sections['$section']['properties']['index']]";
		}

		$this->_parse_modifiers($output, $modifiers);

		return $output;
	}

	function _parse_section_prop($section_prop_expr)
	{
		$modifiers = explode('|', $section_prop_expr);

		preg_match('!%(\w+)\.(\w+)%!', array_shift($modifiers), $match);
		$section_name = $match[1];
		$prop_name = $match[2];

		$output = "\$_sections['$section_name']['properties']['$prop_name']";

		$this->_parse_modifiers($output, $modifiers);

		return $output;
	}

	function _parse_modifiers(&$output, $modifiers)
	{
		foreach ($modifiers as $modifier) {
			$modifier = explode(':', $modifier);
			$modifier_name = array_shift($modifier);

			/*
			 * First we lookup the modifier function name in the registered
			 * modifiers table.
			 */
			$mod_func_name = $this->_modifiers[$modifier_name];

			/*
			 * If we don't find that modifier there, we assume it's just a PHP
			 * function name.
			 */
			/* TODO strict syntax check */
			if (!isset($mod_func_name))
				$mod_func_name = $modifier_name;

			if (count($modifier) > 0)
				$modifier_args = ", ".implode(', ', $modifier);
			else
				$modifier_args = "";

			$output = "$mod_func_name($output$modifier_args)";
		}
	}
	
/*======================================================================*\
	Function:	_read_file()
	Purpose:	read in a file
\*======================================================================*/

	function _read_file($filename)
	{
		if(! ($fd = fopen($filename,"r")))
		{
			$this->_set_errorMsg("problem reading \"$filename.\"");
			return false;
		}
		$contents = fread($fd,filesize($filename));
		fclose($fd);
		return $contents;
	}

/*======================================================================*\
	Function:	_write_file()
	Purpose:	write out a file
\*======================================================================*/

	function _write_file($filename,$contents)
	{
		if(!($fd = fopen($filename,"w")))
		{
			$this->_set_errorMsg("problem writing \"$filename.\"");
			return false;
		}
		fwrite($fd,$contents);
		fclose($fd);
		return true;
	}

/*======================================================================*\
	Function:	_set_errorMsg()
	Purpose:	set the error message
\*======================================================================*/

	function _set_errorMsg($errorMsg)
	{
		$this->errorMsg="smarty error: $errorMsg";
		return true;
	}

}

?>

<?
/*
 * Project:		Smarty: the PHP compiled template engine
 * File:		Smarty.class.php
 * Author:		Monte Ohrt <monte@ispi.net>
 *
 */

require("Smarty.functions.php");

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
			include($compile_file);
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
			if(!file_exists($compile_dir."/".$tpl_file_name) ||
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
		//echo "compiling $compilepath.<br>\n";
		if(!($template_contents = $this->_read_file($filepath)))
			return false;

		/* if(preg_match("/^(.+)\/([^\/]+)$/",$compilepath,$match))
		{
			$ctpl_file_dir = $match[1];			
			$ctpl_file_name = $match[2];
		} */

		if(!$this->allow_php)
		{
			// escape php tags in templates
			$search = array(	"/\<\?/i",
								"/\?\>/i"
							);
			$replace = array(	"&lt;?",
								"?&gt;"
							);
			$template_contents = preg_replace($search,$replace,$template_contents);
		}

		$search = array();
		$replace = array();

		$ld = preg_quote($this->left_delimiter,"/");
		$rd = preg_quote($this->right_delimiter,"/");

		$search[] =			"/^".$ld."\*.*\*".$rd."$/U"; // remove template comments
		$replace[] =		"";
		$search[] =			"/(\\\$[\w\d\_]+)\.rownum\.even\.([\d]+)/"; // replace section rownum.even.digit
		$replace[] =		"((\\1_secvar / \\2) % 2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.rownum\.odd\.([\d]+)/"; // replace section rownum.even.digit
		$replace[] =		"!((\\1_secvar / \\2) % 2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.index\.even\.([\d]+)/"; // replace section index.even.digit
		$replace[] =		"!((\\1_secvar) % \\2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.index\.odd\.([\d]+)/"; // replace section index.even.digit
		$replace[] =		"((\\1_secvar) % \\2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.rownum\.mod\.([\d]+)/"; // replace section rownum.even.digit
		$replace[] =		"!((\\1_secvar+1) % \\2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.index\.mod\.([\d]+)/"; // replace section rownum.even.digit
		$replace[] =		"!((\\1_secvar) % \\2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.rownum\.even/"; // replace section rownum.even
		$replace[] =		"!((\\1_secvar+1) % 2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.rownum\.odd/"; // replace section rownum.odd
		$replace[] =		"((\\1_secvar+1) % 2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.index\.even/"; // replace section index.even
		$replace[] =		"!((\\1_secvar) % 2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.index\.odd/"; // replace section index.odd
		$replace[] =		"((\\1_secvar) % 2)";
		$search[] =			"/(\\\$[\w\d\_]+)\.rownum/"; // replace section rownum
		$replace[] =		"\\1_secvar+1";
		$search[] =			"/(\\\$[\w\d\_]+)\.index/"; // replace section index
		$replace[] =		"\\1_secvar";
		$search[] =			"/(\\\$[\w\d\_]+)\.([\w\d\_]+)/"; // replace section vars
		$replace[] =		"\$\\2[\\1_secvar]";
		$search[] =			"/\beq\b/i"; // replace eq with ==
		$replace[] =		"==";
		$search[] =			"/\blt\b/i"; // replace lt with <
		$replace[] =		"<";
		$search[] =			"/\bgt\b/i"; // replace gt with >
		$replace[] =		">";
		$search[] =			"/\bl(t)?e\b/i"; // replace le or lte with <=
		$replace[] =		"<=";
		$search[] =			"/\bg(t)?e\b/i"; // replace ge or gte with >=
		$replace[] =		">=";
		$search[] =			"/\bne(q)?\b/i"; // replace ne or neq with !=
		$replace[] =		"!=";
		$search[] =			"/\band\b/i"; // replace and with &&
		$replace[] =		"&&";
		$search[] =			"/\bmod\b/i"; // replace mod with %
		$replace[] =		"%";
		$search[] =			"/\bor\b/i"; // replace or with ||
		$replace[] =		"||";
		$search[] =			"/\bnot\b/i"; // replace not with !
		$replace[] =		"!";
		$search[] =			"/^".$ld."if (.*)".$rd."$/Ui"; // replace if tags
		$replace[] =		"<?php if(\\1): ?>";
		$search[] =			"/^".$ld."\s*else\s*".$rd."$/i"; // replace else tags
		$replace[] =		"<?php else: ?>";
		$search[] =			"/^".$ld."\s*\/if\s*".$rd."$/i"; // replace /if tags
		$replace[] =		"<?php endif; ?>";
		$search[] =			"/^".$ld."\s*include\s*\"?([^\s]+)\"?".$rd."$/i";		// replace include tags
		/* $replace[] =		"<?php include(\"".$ctpl_file_dir."/\\1\"); ?>"; */
		$replace[] =		"<?php include(\"./".$this->template_dir.$this->compile_dir_ext."/\\1\"); ?>";
		$search[] =			"/^".$ld."\s*section\s+name\s*=\s*\"?([\w\d\_]+)\"?\s+\\\$([^\s]+)\s*".$rd."$/i"; // replace section tags
		$replace[] =		"<?php for(\$\\1_secvar=0; \$\\1_secvar<count(\$\\2); \$\\1_secvar++): ?>";
		$search[] =			"/^".$ld."\s*\/section\s*".$rd."$/i"; // replace /section tags
		$replace[] =		"<?php endfor; ?>";
		// registered functions
		if(count($this->registered_functions) > 0)
		{
			$funcs = implode("|",$this->registered_functions);
			// user functions without args
			$search[]  = 	"/^".$ld."\s*(".$funcs.")\s*".$rd."$/i";
			$replace[] =	"<?php smarty_\\1(); ?>";
			// user functions with args
			$search[]  = 	"/^".$ld."\s*(".$funcs.")\s+(.*)".$rd."$/i";
			$replace[] =	"<?php smarty_\\1(\\2); ?>";
		}			
		$search[] =			"/^".$ld."\s*(\\\$[^\s]+)\s*".$rd."$/";	// replace vars							
		$replace[] =		"<?php print \\1; ?>";

		// collect all the tags in the template
				
		preg_match_all("/".$ld.".*".$rd."/U",$template_contents,$match);
		$template_tags = $match[0];
		
		$template_tags_modified = preg_replace($search,$replace,$template_tags);

		// replace the tags in the template
		for($tagloop=0; $tagloop<count($template_tags); $tagloop++)
			$template_contents = preg_replace("/".preg_quote($template_tags[$tagloop],"/")."/",$template_tags_modified[$tagloop],$template_contents);

		
		// reformat data within {strip}{/strip} tags, removing spaces, tabs and newlines
		preg_match_all("/\{strip\}.*\{\/strip\}/Usi",$template_contents,$match);
		$strip_tags = $match[0];
			
		$search = array( "/\{\/?strip\}/i","/[\t ]+$/m","/^[\t ]+/m","/[\r\n]+/" );
		$replace = array ("","",""," ");
		$strip_tags_modified = preg_replace($search,$replace,$strip_tags);
				
		// replace the tags in the template
		for($tagloop=0; $tagloop<count($strip_tags); $tagloop++)
			$template_contents = preg_replace("/".preg_quote($strip_tags[$tagloop],"/")."/",$strip_tags_modified[$tagloop],$template_contents);

		// replace literal delimiter tags
		$template_contents = preg_replace("/{ldelim}/",$this->left_delimiter,$template_contents);
		$template_contents = preg_replace("/{rdelim}/",$this->right_delimiter,$template_contents);
								
		if(!($this->_write_file($compilepath,$template_contents)))
			return false;
		
		return true;
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

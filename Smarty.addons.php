<?

/*
 * Project:		Smarty: the PHP compiled template engine
 * File:		Smarty.functions.php
 * Author:		Monte Ohrt <monte@ispi.net>
 *
 * This is the smarty custom function file.
 * To add your own functions, name the function
 * smarty_[funcname], then add the function name
 * to the $registered_functions array in the
 * smarty.class.php file. You may then call your
 * function from a template file like so: {funcname [$var]...}
 */


/*======================================================================*\
	Function:	htmlesc
	Purpose:	html escape template vars
\*======================================================================*/

function smarty_htmlesc($var)
{
	print htmlspecialchars($var);
}

/*======================================================================*\
	Function:	urlesc
	Purpose:	URL escape template vars
\*======================================================================*/

function smarty_urlesc($var)
{
	print urlencode($var);
}

/*======================================================================*\
	Function:	default
	Purpose:	display a default value for empty template vars
\*======================================================================*/

function smarty_default($var,$default_val)
{
	if(empty($var))
		print $default_val;
	else
		print $var;
}

/*======================================================================*\
	Function:	configload
	Purpose:	load vars from a config file

	Notes:		config files must be in this format:
	
				key = "val"
				key2 = "val2"
\*======================================================================*/

function smarty_configload($config_file)
{

	global $_config_vars;

	if(!is_array($_config_vars))
		$_config_vars = array();
		
	// only load in the config file
	if(! ($fd = fopen($config_file,"r")))
	{
		print ("<!-- problem reading \"$config_file.\" -->");
		return false;
	}
	$config_contents = fread($fd,filesize($config_file));
	fclose($fd);

	$contents_array = preg_split("/[\r\n]+/", $config_contents);

	// read in the variables from the config file
	foreach($contents_array as $current_line)
	{
		if (preg_match("/^\w+/",$current_line))
		{
			if(preg_match("/^([^\=]+)=(.*)/", $current_line, $preg_results))
			{
				$config_key = $preg_results[1];
				$config_val = trim($preg_results[2]);

				// remove tabs and spaces from key
				$key = preg_replace("/[ \t]/", "", $config_key);

				// is value surrounded by quotes?
				if (!preg_match("/^['\"]/", $config_val))
				{
					$val = $config_val;
				}
				else
				{
					// Strip the leading and trailing quotes
					$val = preg_replace("/^['\"]/","",$config_val);
					$val = preg_replace("/['\"]$/","",$val);
				}

				// store the key and val in the config array
				$_config_vars[$key] = $val;
			}
		}
	}
	
	return true;
		
}

/*======================================================================*\
	Function:	configclear
	Purpose:	clear config vars
\*======================================================================*/

function smarty_configclear()
{

	global $_config_vars;
	$_config_vars = array();
	
	return true;
		
}

/*======================================================================*\
	Function:	configprint
	Purpose:	print a var from config
\*======================================================================*/

function smarty_configprint($var)
{

	global $_config_vars;
	
	if (isset($_config_vars[$var]))
		print $_config_vars[$var];
	else
		print "<!-- no value for $var in $config_file -->";
	
	return true;
		
}

/*======================================================================*\
	Function:	configset
	Purpose:	set a var from config
\*======================================================================*/

function smarty_configset($var,&$setvar)
{

	global $_config_vars;
	
	if (isset($_config_vars[$var]))
		$setvar = $_config_vars[$var];
	else
		return false;
			
	return true;
		
}


?>

<?php
/*
 * Project:     Smarty: the PHP compiled template engine
 * File:        Smarty.addons.php
 * Author:      Monte Ohrt <monte@ispi.net>
 *              Andrei Zmievski <andrei@php.net>
 * Version:     1.4.2
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
 * You may contact the authors of Smarty by e-mail at:
 * monte@ispi.net
 * andrei@php.net
 *
 * Or, write to:
 * Monte Ohrt
 * CTO, ispi
 * 237 S. 70th suite 220
 * Lincoln, NE 68510
 *
 * The latest version of Smarty can be obtained from:
 * http://www.phpinsider.com
 *
 */


/*============================================*\
  Inserts handler
\*============================================*/

function _smarty_insert_handler($args, $caching, $delimiter)
{
    if ($caching) {
        $arg_string = serialize($args);
        return "$delimiter{insert_cache $arg_string}$delimiter";
    } else {
        $function_name = 'insert_'.$args['name'];
        return $function_name($args);
    }
}


/*============================================*\
  Modifiers
\*============================================*/

function _smarty_mod_handler()
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
    Function: smarty_mod_escape
    Purpose:  Escape the string according to escapement type
\*======================================================================*/
function smarty_mod_escape($string, $esc_type = 'html')
{
    switch ($esc_type) {
        case 'html':
            return htmlspecialchars($string);

        case 'url':
            return urlencode($string);

        default:
            return $string;
    }
}


/*======================================================================*\
    Function: smarty_mod_truncate
    Purpose:  Truncate a string to a certain length if necessary,
              optionally splitting in the middle of a word, and
              appending the $etc string.
\*======================================================================*/
function smarty_mod_truncate($string, $length = 80, $etc = '...', $break_words = false)
{
    if ($length == 0)
        return '';

    if (strlen($string) > $length) {
        $length -= strlen($etc);
        $fragment = substr($string, 0, $length+1);
        if ($break_words)
            $fragment = substr($fragment, 0, -1);
        else
            $fragment = preg_replace('/\s+(\S+)?$/', '', $fragment);
        return $fragment.$etc;
    } else
        return $string;
}


/*======================================================================*\
    Function: smarty_mod_spacify
    Purpose:  add spaces between characters in a string
\*======================================================================*/
function smarty_mod_spacify($string, $spacify_char = ' ')
{
    return implode($spacify_char, preg_split('//', $string, -1, PREG_SPLIT_NO_EMPTY));
}

/*======================================================================*\
    Function: smarty_mod_date_format
    Purpose:  format datestamps via strftime
\*======================================================================*/
function smarty_mod_date_format($string, $format="%b %e, %Y")
{
    return strftime($format, smarty_make_timestamp($string));
}

/*======================================================================*\
    Function: smarty_make_timestamp
    Purpose:  used by other smarty functions to make a timestamp
              from a string of characters.
\*======================================================================*/
function smarty_make_timestamp($string)
{        
    $time = strtotime($string);
    if(is_numeric($time) && $time != -1)
        return $time;
    
    // is mysql timestamp format of YYYYMMDDHHMMSS?
    if(is_numeric($string) && strlen($string) == 14) {
        $time = mktime(substr($string,8,2),substr($string,10,2),substr($string,12,2),
               substr($string,4,2),substr($string,6,2),substr($string,0,4));
        
        return $time;
    }

    // can decipher, must be timestamp already?    
    return $string;
}

/*======================================================================*\
    Function: smarty_mod_string_format
    Purpose:  format strings via sprintf
\*======================================================================*/
function smarty_mod_string_format($string, $format)
{
    return sprintf($format, $string);
}

/*======================================================================*\
    Function: smarty_mod_replace
    Purpose:  simple search/replace
\*======================================================================*/
function smarty_mod_replace($string, $search, $replace)
{
    return str_replace($search, $replace, $string);
}

/*======================================================================*\
    Function: smarty_mod_regex_replace
    Purpose:  regular epxression search/replace
\*======================================================================*/
function smarty_mod_regex_replace($string, $search, $replace)
{
    return preg_replace($search, $replace, $string);
}

/*======================================================================*\
    Function: smarty_mod_strip_tags
    Purpose:  strip html tags from text
\*======================================================================*/
function smarty_mod_strip_tags($string, $replace_with_space = true)
{
    if ($replace_with_space)
        return preg_replace('!<[^>]*?>!', ' ', $string);
    else
        return strip_tags($string);
}

/*======================================================================*\
    Function: smarty_mod_default
    Purpose:  designate default text for empty variables
\*======================================================================*/
function smarty_mod_default($string, $default="")
{
    if(empty($string))
        return $default;
    else
        return $string;
}

/*======================================================================*\
    Function: smarty_func_assign
    Purpose:  assign a value to a template variable
\*======================================================================*/
function smarty_func_assign($args, &$smarty_obj)
{
	extract($args);

	if (empty($var)) {
    	$smarty_obj->_trigger_error_msg("assign: missing 'var' parameter");
    	return;
	}

	if (empty($value)) {
    	$smarty_obj->_trigger_error_msg("assign: missing 'value' parameter");
    	return;
	}

	$smarty_obj->assign($var, $value);
	return true;
}

/*============================================*\
  Custom tag functions
\*============================================*/

/*======================================================================*\
    Function: smarty_func_html_options
    Purpose:  Prints the list of <option> tags generated from
              the passed parameters
\*======================================================================*/
function smarty_func_html_options()
{
    $print_result = true;

    extract(func_get_arg(0));

    $html_result = "";

    settype($selected, 'array');
    if (isset($options)) {
        settype($options, 'array');
        foreach ($options as $key => $value) {
            $html_result .= "<option value=\"$key\"";
            if (in_array($key, $selected))
                $html_result .= " selected";
            $html_result .= ">$value</option>\n";
        }
    } else {
        settype($output, 'array');
        settype($values, 'array');
        for ($i = 0; $i < count($output); $i++) {
            /* By default, check value against $selected */
            $sel_check = $values[$i];
            $html_result .= "<option";
            if ($i < count($values))
                $html_result .= " value=\"".$values[$i]."\"";
            else
                $sel_check = $output[$i];       /* if more outputs than values, then
                                                   check output against $selected */        
            if (in_array($sel_check, $selected))
                $html_result .= " selected";
            $html_result .= ">".$output[$i]."</option>\n";
        }
    }

    if ($print_result)
        print $html_result;
    else
        return $html_result;
}


/*======================================================================*\
    Function: smarty_func_html_select_date
    Purpose:  Prints the dropdowns for date selection.
\*======================================================================*/
function smarty_func_html_select_date()
{
    /* Default values. */
    $prefix         = "Date_";
    $time           = time();
    $start_year     = strftime("%Y");
    $end_year       = $start_year;
    $display_days   = true;
    $display_months = true;
    $display_years  = true;
    $month_format   = "%B";
    $day_format     = "%02d";
    $year_as_text   = false;
    
    extract(func_get_arg(0));

    $time = smarty_make_timestamp($time);
        
    $html_result = "";

    if ($display_months) {
        $month_names = array();

        for ($i = 1; $i <= 12; $i++)
            $month_names[] = strftime($month_format, mktime(0, 0, 0, $i, 1, 2000));

        $html_result .= '<select name="'.$prefix.'Month">'."\n";
        $html_result .= smarty_func_html_options(array('output'     => $month_names,
                                                       'values'     => range(1, 12),
                                                       'selected'   => strftime("%m", $time),
                                                       'print_result' => false));
        $html_result .= "</select>\n";
    }

    if ($display_days) {
        $days = range(1, 31);
        for ($i = 0; $i < count($days); $i++)
            $days[$i] = sprintf($day_format, $days[$i]);

        $html_result .= '<select name="'.$prefix.'Day">'."\n";
        $html_result .= smarty_func_html_options(array('output'     => $days,
                                                       'values'     => range(1, 31),
                                                       'selected'   => strftime("%d", $time),
                                                       'print_result' => false));
        $html_result .= "</select>\n";
    }

    if ($display_years) {
        if ($year_as_text) {
            $html_result .= '<input type="text" name="'.$prefix.'Year" value="'.strftime('%Y', $time).'" size="4" maxlength="4">';
        } else {
            $years = range($start_year, $end_year);

            $html_result .= '<select name="'.$prefix.'Year">'."\n";
            $html_result .= smarty_func_html_options(array('output' => $years,
                                                           'values' => $years,
                                                           'selected'   => strftime("%Y", $time),
                                                           'print_result' => false));
            $html_result .= "</select>\n";
        }
    }

    print $html_result;
}


/*======================================================================*\
    Function: smarty_func_html_select_time
    Purpose:  Prints the dropdowns for time selection
\*======================================================================*/
function smarty_func_html_select_time()
{
    /* Default values. */
    $prefix             = "Time_";
    $time               = time();
    $display_hours        = true;
    $display_minutes    = true;
    $display_seconds    = true;
    $display_meridian   = true;
    $use_24_hours        = true;
    $minute_interval    = 1;
    $second_interval    = 1;
    
    extract(func_get_arg(0));
    
    $time = smarty_make_timestamp($time);

    $html_result = '';
    
    if ($display_hours) {
        $hours       = $use_24_hours ? range(0, 23) : range(1, 12);
        $hour_fmt = $use_24_hours ? '%H' : '%I';
        for ($i = 0; $i < count($hours); $i++)
            $hours[$i] = sprintf('%02d', $hours[$i]);
        $html_result .= '<select name="'.$prefix.'Hour">'."\n";
        $html_result .= smarty_func_html_options(array('output'          => $hours,
                                                       'values'          => $hours,
                                                       'selected'      => strftime($hour_fmt, $time),
                                                       'print_result' => false));
        $html_result .= "</select>\n";
    }
    
    if ($display_minutes) {
        $all_minutes = range(0, 59);
        for ($i = 0; $i < count($all_minutes); $i+= $minute_interval)
            $minutes[] = sprintf('%02d', $all_minutes[$i]);
        $selected = intval(floor(strftime('%M', $time) / $minute_interval) * $minute_interval);
        $html_result .= '<select name="'.$prefix.'Minute">'."\n";
        $html_result .= smarty_func_html_options(array('output'          => $minutes,
                                                       'values'          => $minutes,
                                                       'selected'      => $selected,
                                                       'print_result' => false));
        $html_result .= "</select>\n";
    }
    
    if ($display_seconds) {
        $all_seconds = range(0, 59);
        for ($i = 0; $i < count($all_seconds); $i+= $second_interval)
            $seconds[] = sprintf('%02d', $all_seconds[$i]);
        $selected = intval(floor(strftime('%S', $time) / $second_interval) * $second_interval);
        $html_result .= '<select name="'.$prefix.'Second">'."\n";
        $html_result .= smarty_func_html_options(array('output'          => $seconds,
                                                       'values'          => $seconds,
                                                       'selected'      => $selected,
                                                       'print_result' => false));
        $html_result .= "</select>\n";
    }
    
    if ($display_meridian && !$use_24_hours) {
        $html_result .= '<select name="'.$prefix.'Meridian">'."\n";
        $html_result .= smarty_func_html_options(array('output'          => array('AM', 'PM'),
                                                       'values'          => array('am', 'pm'),
                                                       'selected'      => strtolower(strftime('%p', $time)),
                                                       'print_result' => false));
        $html_result .= "</select>\n";
    }
    
    print $html_result;
}


/*======================================================================*\
    Function: smarty_func_math
    Purpose:  allow math computations in template
\*======================================================================*/
function smarty_func_math($args, $smarty_obj) {
        // be sure equation parameter is present
        if(empty($args["equation"])) {
            $smarty_obj->_trigger_error_msg("math: missing equation parameter");
            return;
        }
                
        $equation = $args["equation"];
        
        // make sure parenthesis are balanced    
        if(substr_count($equation,"(") != substr_count($equation,")")) {
            $smarty_obj->_trigger_error_msg("math: unbalanced parenthesis");
            return;
        }
                        
        // match all vars in equation, make sure all are passed
        preg_match_all("![a-zA-Z][a-zA-Z0-9]*!",$equation,$match);
        
        foreach($match[0] as $curr_var) {
            if(!in_array($curr_var,array_keys($args)) &&
                    !in_array($curr_var,
                              array('int','abs','ceil','cos','exp','floor','log','log10',
                                    'max','min','pi','pow','rand','round','sin','sqrt','srand','tan'))) {
                $smarty_obj->_trigger_error_msg("math: parameter $curr_var not passed as argument");
                return;
            }       
        }
       
        foreach($args as $key => $val) {
            if($key != "equation" && $key != "format") {
                // make sure value is not empty
                if(strlen($val)==0) {
                    $smarty_obj->_trigger_error_msg("math: parameter $key is empty");
                    return;                                        
                }
                if(!is_numeric($val)) {
                    $smarty_obj->_trigger_error_msg("math: parameter $key: is not numeric");
                    return;                    
                }
                $equation = preg_replace("/\b$key\b/",$val,$equation);       
            }
        }
               
    eval("\$smarty_math_result = ".$equation.";");

    if(empty($args["format"]))
        echo $smarty_math_result;
    else
        printf($args["format"],$smarty_math_result);
}

/*======================================================================*\
    Function: smarty_func_fetch
    Purpose:  fetch file, web or ftp data and display results
\*======================================================================*/
function smarty_func_fetch($args, &$smarty_obj) {
    extract($args);
	
    if (empty($file)) {
        $smarty_obj->_trigger_error_msg("parameter 'file' cannot be empty");
        return;                    
    }

	if ($smarty_obj->security && !preg_match('!^(http|ftp)://!', $file)) {
		// make sure fetched file comes from secure directory
		foreach ($smarty_obj->secure_dir as $curr_dir) {
			if (substr(realpath($file), 0, strlen(realpath($curr_dir))) == realpath($curr_dir)) {
				$resource_is_secure = true;
				break;
			}
		}
		if (!$resource_is_secure) {
        	$smarty_obj->_trigger_error_msg("(secure mode) fetching '$file' is not allowed");
        	return;
		}				
	}

    readfile($file);
}

/*======================================================================*\
    Function: smarty_mod_count_characters
    Purpose:  count the number of characters in a text
\*======================================================================*/
function smarty_mod_count_characters($string,$include_spaces=false) {
    
    if($include_spaces)
       return(strlen($string));
            
    return preg_match_all("/[^\s]/",$string,$match);
}

/*======================================================================*\
    Function: smarty_mod_count_words
    Purpose:  count the number of words in a text
\*======================================================================*/
function smarty_mod_count_words($string) {
    
    // split text by ' ',\r,\n,\f,\t
    $split_array = preg_split("/\s+/",$string);
    // count matches that contain alphanumerics
    $word_count = preg_grep("/[a-zA-Z0-9]/",$split_array);
    
    return count($word_count);
}

/*======================================================================*\
    Function: smarty_mod_count_sentences
    Purpose:  count the number of sentences in a text
\*======================================================================*/
function smarty_mod_count_sentences($string,$include_spaces=false) {
    
    // find periods with a word before but not after.    
    return preg_match_all("/[^\s]\.(?!\w)/",$string,$match);
    
}

/*======================================================================*\
    Function: smarty_mod_count_paragraphs
    Purpose:  count the number of sentences in a text
\*======================================================================*/
function smarty_mod_count_paragraphs($string,$include_spaces=false) {
    
    // count \r or \n characters
    return count( preg_split("/[\r\n]+/",$string) );
}

/*======================================================================*\
    Function: smarty_func_counter
    Purpose:  print out a counter value
\*======================================================================*/
function smarty_func_counter() {
    
	static $count = array();
	static $skipval = array();
	static $dir = array();
	static $id = "default";
	static $printval = array();
	
    extract(func_get_arg(0));

	if(!isset($id))
		$id = "default";
	
	if(isset($start))
		$count[$id] = $start;	
	elseif(!isset($count[$id]))
		$count[$id]=1;

	if(!isset($print))
		$printval[$id]=true;
	else
		$printval[$id]=$print;

	if($printval[$id])
		echo $count[$id];
	
	if(isset($skip))
		$skipval[$id] = $skip;
	elseif(!isset($skipval))
		$skipval[$id] = 1;
	
	if(isset($direction))
		$dir[$id] = $direction;
	elseif(!isset($dir[$id]))
		$dir[$id] = "up";
	
	if($dir[$id] == "down")
		$count[$id] -= $skipval[$id];
	else
		$count[$id] += $skipval[$id];

    return true;
}

/*======================================================================*\
    Function: smarty_func_assign_debug_info
    Purpose:  assign debug info to the template
\*======================================================================*/
function smarty_func_assign_debug_info($args, &$smarty_obj) {
	$assigned_vars = $smarty_obj->_tpl_vars;
	ksort($assigned_vars);
	$included_templates = $smarty_obj->_included_tpls;
	//sort($included_templates);
	$smarty_obj->assign("_debug_keys",array_keys($assigned_vars));
	$smarty_obj->assign("_debug_vals",array_values($assigned_vars));
	$smarty_obj->assign("_debug_tpls",$included_templates);
	return true;
}

/* vim: set expandtab: */

?>

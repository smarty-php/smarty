<?php

$smarty_plugin_info['name'] = 'standard';


$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'lower',
                                       'impl' => 'strtolower');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'upper',
                                       'impl' => 'strtoupper');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'capitalize',
                                       'impl' => 'ucwords');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'escape',
                                       'impl' => 'smarty_mod_escape');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'truncate',
                                       'impl' => 'smarty_mod_truncate');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'spacify',
                                       'impl' => 'smarty_mod_spacify');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'date_format',
                                       'impl' => 'smarty_mod_date_format');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'string_format',
                                       'impl' => 'smarty_mod_string_format');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'replace',
                                       'impl' => 'smarty_mod_replace');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'strip_tags',
                                       'impl' => 'smarty_mod_strip_tags');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_MODIFIER,
                                       'name' => 'default',
                                       'impl' => 'smarty_mod_default');


$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_FUNCTION,
                                       'name' => 'html_options',
                                       'impl' => 'smarty_func_html_options');
$smarty_plugin_info['table'][] = array('type' => SMARTY_PLUGIN_FUNCTION,
                                       'name' => 'html_select_date',
                                       'impl' => 'smarty_func_html_select_date');


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
    Purpose:  Insert a character (space by default) between every
              character in the string.
\*======================================================================*/
function smarty_mod_spacify($string, $spacify_char = ' ')
{
    return implode($spacify_char, preg_split('//', $string, -1, PREG_SPLIT_NO_EMPTY));
}


/*======================================================================*\
    Function: smarty_mod_date_format
    Purpose:  Output formatted date
\*======================================================================*/
function smarty_mod_date_format($string, $format="%b %e, %Y")
{
    return strftime($format, $string);
}


/*======================================================================*\
    Function: smarty_mod_string_format
    Purpose:  Output formatted string
\*======================================================================*/
function smarty_mod_string_format($string, $format)
{
    return sprintf($format, $string);
}


/*======================================================================*\
    Function: smarty_mod_replace
    Purpose:  Perform simple search and replace
\*======================================================================*/
function smarty_mod_replace($string, $search, $replace)
{
    return str_replace($search, $replace, $string);
}


/*======================================================================*\
    Function: smarty_mod_strip_tags
    Purpose:  Strip HTML tags
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
    Purpose:  Output default value if string is empty
\*======================================================================*/
function smarty_mod_default($string, $default='')
{
    if(empty($string))
        return $default;
    else
        return $string;
}


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
        array_walk($days, create_function('&$x', '$x = sprintf("'.$day_format.'", $x);'));

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

/* vim: set expandtab: */

?>

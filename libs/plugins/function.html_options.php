<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     html_options
 * Purpose:  Prints the list of <option> tags generated from
 *           the passed parameters
 * -------------------------------------------------------------
 */
function smarty_function_html_options($params, &$smarty)
{
    extract($params);

    $html_result = '';

    settype($selected, 'array');
    if (isset($options)) {
        settype($options, 'array');
        foreach ($options as $key => $value) {
			$html_result .= smarty_function_html_options_optoutput($key, $value, $selected);
        }
    } else {
        settype($output, 'array');
        settype($values, 'array');
        for ($i = 0, $for_max = count($output); $i < $for_max; $i++) {
            if ($i < count($values)) {
				$html_result .= smarty_function_html_options_optoutput($values[$i], $output[$i], $selected);
			} else {
				$html_result .= smarty_function_html_options_optoutput($output[$i], $output[$i], $selected);
			}
        }
    }

    return $html_result;
}

function smarty_function_html_options_optoutput($key, $value, $selected) {
	if(!is_array($value)) {
    	$html_result = '<option label="' . smarty_function_html_options_htmlspecialchars($value) . '" value="' . 
				smarty_function_html_options_htmlspecialchars($key) . '"';
    	if (in_array($key, $selected))
        	$html_result .= " selected=\"selected\"";
    	$html_result .= '>' . smarty_function_html_options_htmlspecialchars($value) . '</option>' . "\n";
	} else {
		$html_result = smarty_function_html_options_optgroup($key, $value, $selected);
	}
		return $html_result;	
}

function smarty_function_html_options_optgroup($key, $values, $selected) {
	$optgroup_html = '<optgroup label="' . smarty_function_html_options_htmlspecialchars($value) . '">' . "\n";
	foreach ($values as $key => $value) {
		$optgroup_html .= smarty_function_html_options_optoutput($key, $value, $selected);
	}
	$optgroup_html .= "</optgroup>\n";
	return $optgroup_html;
}

function smarty_function_html_options_htmlspecialchars($text) {
	// do not escape already escaped entities (&amp; &#123;)
	$text = preg_replace('!&(#?\w+);!', '%%%SMARTY_START%%%\\1%%%SMARTY_END%%%', $text);
	$text = htmlspecialchars($text);
	$text = str_replace(array('%%%SMARTY_START%%%','%%%SMARTY_END%%%'), array('&',';'), $text);
	return $text;
}

/* vim: set expandtab: */

?>

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
    require_once $smarty->_get_plugin_filepath('shared','escape_special_chars');

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

    /* passthru all unknown params to $extra */
    $extra = '';
    unset($params['values']);
    unset($params['output']);
    unset($params['selected']);
    unset($params['options']);
    unset($params['name']);
    foreach ($params as $key=>$value) $extra .= ' ' . $key . '="'. htmlspecialchars($value) . '"';

    if(!empty($name)) {
        $html_result = '<select name="' . $name . '"' . $extra . '>' . "\n" . $html_result . '</select>' . "\n";
    }
    return $html_result;
}

function smarty_function_html_options_optoutput($key, $value, $selected) {
	if(!is_array($value)) {
    	$html_result = '<option label="' . smarty_function_escape_special_chars($value) . '" value="' . 
				smarty_function_escape_special_chars($key) . '"';
    	if (in_array($key, $selected))
        	$html_result .= " selected=\"selected\"";
    	$html_result .= '>' . smarty_function_escape_special_chars($value) . '</option>' . "\n";
	} else {
		$html_result = smarty_function_html_options_optgroup($key, $value, $selected);
	}
		return $html_result;	
}

function smarty_function_html_options_optgroup($key, $values, $selected) {
	$optgroup_html = '<optgroup label="' . smarty_function_escape_special_chars($value) . '">' . "\n";
	foreach ($values as $key => $value) {
		$optgroup_html .= smarty_function_html_options_optoutput($key, $value, $selected);
	}
	$optgroup_html .= "</optgroup>\n";
	return $optgroup_html;
}

/* vim: set expandtab: */

?>

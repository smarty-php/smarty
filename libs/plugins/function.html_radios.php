<?php

/*
 * Smarty plugin
 * ---------------------------------------------------------------------------------------------
 * Type:     	function
 * Name:     	html_radios
 * Purpose:  	Prints the list of <input type="radio" tags generated from the passed parameters
 * Parameters:	name [optional]- string default "radio"
 * 				checked [optional]- string default not set
 * 				values [required] - array
 * 				separator[optional] - ie <br> or &nbsp;
 * 				output[optional] - without this one the buttons don't have names
 * Author:		Christopher Kvarme <christopher.kvarme@flashjab.com> 
 * ---------------------------------------------------------------------------------------------
 */
function smarty_function_html_radios($params, &$smarty)
{
    extract($params);

    $html_result = '';
	if(!isset($name)){
	$name = "radio";
	}
    settype($checked, 'array');
    if (isset($radios)) {
        settype($radios, 'array');
        foreach ($radios as $key => $value) {
			$html_result .= smarty_function_html_radios_optoutput($key, $value, $checked);
        }
    } else {
        settype($output, 'array');
        settype($values, 'array');
        for ($i = 0, $for_max = count($output); $i < $for_max; $i++) {
            if ($i < count($values)) {
				$html_result .= smarty_function_html_radios_optoutput($values[$i], $output[$i], $checked, $name, $separator);
			} else {
				$html_result .= smarty_function_html_radios_optoutput($output[$i], $output[$i], $checked, $name, $separator);
			}
        }
    }

    return $html_result;
}

function smarty_function_html_radios_optoutput($key, $value, $checked, $name, $separator) {
	if(!is_array($value)) {
    	$html_result = '<input type="radio" name="' . htmlspecialchars($name) . '" value="' . 
				htmlspecialchars($key) . '"';
    	if (in_array($key, $checked))
        	$html_result .= " checked";
    	$html_result .= '>' . htmlspecialchars($value) . $separator . "\n";
	} else {
		$html_result = smarty_function_html_radios_optgroup($key, $value, $checked, $name, $separator);
	}
		return $html_result;	
}

function smarty_function_html_radios_optgroup($key, $values, $checked) {
	$optgroup_html = '<optgroup label="' . htmlspecialchars($value) . '">' . "\n";
	foreach ($values as $key => $value) {
		$optgroup_html .= smarty_function_html_radios_optoutput($key, $value, $checked);
	}
	$optgroup_html .= "</optgroup>\n";
	return $optgroup_html;
}

/* vim: set expandtab: */

?>

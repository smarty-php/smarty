<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * File:       function.html_radios.php
 * Type:       function
 * Name:       html_radios	
 * Version:    1.0
 * Date:       24.Feb.2003
 * Purpose:    Prints out a list of radio input types
 * Input:      name       (optional) - string default "radio"
 *             values     (required) - array
 *             radios     (optional) - associative array
 *             checked    (optional) - array default not set
 *             separator  (optional) - ie <br> or &nbsp;
 *             output     (optional) - without this one the buttons don't have names
 * Author:     Christopher Kvarme <christopher.kvarme@flashjab.com>
 * Credits:    Monte Ohrt <monte@ispi.net>
 * Examples:   {html_radios values=$ids output=$names}
 *             {html_radios values=$ids name='box' separator='<br>' output=$names}
 *             {html_radios values=$ids checked=$checked separator='<br>' output=$names}
 * -------------------------------------------------------------
 */
require_once $this->_get_plugin_filepath('shared','escape_special_chars');
function smarty_function_html_radios($params, &$smarty)
{
    extract($params);

    $_html_result = '';
	if(!isset($name)){
	$name = 'radio';
	}
    settype($checked, 'array');
    if (isset($radios)) {
        settype($radios, 'array');
        foreach ($radios as $_key => $_val) {
			$_html_result .= smarty_function_html_radios_output($name, $_key, $_val, $checked, $separator);
        }
    } else {
        settype($output, 'array');
        settype($values, 'array');
        for ($_i = 0, $_for_max = count($output); $_i < $_for_max; $_i++) {
			$_html_result .= smarty_function_html_radios_output($name, $values[$_i], $output[$_i], $checked, $separator);
        }
    }

    return $_html_result;
}

function smarty_function_html_radios_output($name, $value, $output, $checked, $separator) {
	$_output = '<input type="radio" name="' . smarty_function_escape_special_chars($name) . '[]' .'" value="' . smarty_function_escape_special_chars($value) . '"';
	
    if (in_array($value, $checked)) {
       	$_output .= " checked=\"checked\"";
	}
    $_output .= '>' . $output . $separator . "\n";

	return $_output;	
}


?>

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
    $print_result = true;

    extract($params);

    $html_result = '';

    settype($selected, 'array');
    if (isset($options)) {
        settype($options, 'array');
        foreach ($options as $key => $value) {
            $html_result .= "<option value=\"$key\"";
            if (in_array($key, $selected))
                $html_result .= " selected=\"selected\"";
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
                $html_result .= " selected=\"selected\"";
            $html_result .= ">".$output[$i]."</option>\n";
        }
    }

    if ($print_result)
        print $html_result;
    else
        return $html_result;
}

/* vim: set expandtab: */

?>

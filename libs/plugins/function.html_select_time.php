<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     html_select_time
 * Purpose:  Prints the dropdowns for time selection
 * -------------------------------------------------------------
 */
require_once SMARTY_DIR . $this->plugins_dir . '/shared.make_timestamp.php';
require_once SMARTY_DIR . $this->plugins_dir . '/function.html_options.php';
function smarty_function_html_select_time($params, &$smarty)
{
    /* Default values. */
    $prefix             = "Time_";
    $time               = time();
    $display_hours      = true;
    $display_minutes    = true;
    $display_seconds    = true;
    $display_meridian   = true;
    $use_24_hours       = true;
    $minute_interval    = 1;
    $second_interval    = 1;
    /* Should the select boxes be part of an array when returned from PHP?
       e.g. setting it to "birthday", would create "birthday[Hour]",
       "birthday[Minute]", "birthday[Seconds]" & "birthday[Meridian]".
       Can be combined with prefix. */
    $field_array        = null;

    extract($params);

    $time = smarty_make_timestamp($time);

    $html_result = '';

    if ($display_hours) {
        $hours       = $use_24_hours ? range(0, 23) : range(1, 12);
        $hour_fmt = $use_24_hours ? '%H' : '%I';
        for ($i = 0; $i < count($hours); $i++)
            $hours[$i] = sprintf('%02d', $hours[$i]);
        $html_result .= '<select name=';
        if (null !== $field_array) {
            $html_result .= '"' . $field_array . '[' . $prefix . 'Hour]">'."\n";
        } else {
            $html_result .= '"' . $prefix . 'Hour">'."\n";
        }
        $html_result .= smarty_function_html_options(array('output'          => $hours,
                                                           'values'          => $hours,
                                                           'selected'      => strftime($hour_fmt, $time),
                                                           'print_result' => false),
                                                     $smarty);
        $html_result .= "</select>\n";
    }

    if ($display_minutes) {
        $all_minutes = range(0, 59);
        for ($i = 0; $i < count($all_minutes); $i+= $minute_interval)
            $minutes[] = sprintf('%02d', $all_minutes[$i]);
        $selected = intval(floor(strftime('%M', $time) / $minute_interval) * $minute_interval);
        $html_result .= '<select name=';
        if (null !== $field_array) {
            $html_result .= '"' . $field_array . '[' . $prefix . 'Minute]">'."\n";
        } else {
            $html_result .= '"' . $prefix . 'Minute">'."\n";
        }
        $html_result .= smarty_function_html_options(array('output'          => $minutes,
                                                           'values'          => $minutes,
                                                           'selected'      => $selected,
                                                           'print_result' => false),
                                                     $smarty);
        $html_result .= "</select>\n";
    }

    if ($display_seconds) {
        $all_seconds = range(0, 59);
        for ($i = 0; $i < count($all_seconds); $i+= $second_interval)
            $seconds[] = sprintf('%02d', $all_seconds[$i]);
        $selected = intval(floor(strftime('%S', $time) / $second_interval) * $second_interval);
        $html_result .= '<select name=';
        if (null !== $field_array) {
            $html_result .= '"' . $field_array . '[' . $prefix . 'Second]">'."\n";
        } else {
            $html_result .= '"' . $prefix . 'Second">'."\n";
        }
        $html_result .= smarty_function_html_options(array('output'          => $seconds,
                                                           'values'          => $seconds,
                                                           'selected'      => $selected,
                                                           'print_result' => false),
                                                     $smarty);
        $html_result .= "</select>\n";
    }

    if ($display_meridian && !$use_24_hours) {
        $html_result .= '<select name=';
        if (null !== $field_array) {
            $html_result .= '"' . $field_array . '[' . $prefix . 'Meridian]">'."\n";
        } else {
            $html_result .= '"' . $prefix . 'Meridian">'."\n";
        }
        $html_result .= smarty_function_html_options(array('output'          => array('AM', 'PM'),
                                                           'values'          => array('am', 'pm'),
                                                           'selected'      => strtolower(strftime('%p', $time)),
                                                           'print_result' => false),
                                                     $smarty);
        $html_result .= "</select>\n";
    }

    print $html_result;
}

/* vim: set expandtab: */

?>

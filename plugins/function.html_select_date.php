<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     html_select_date
 * Purpose:  Prints the dropdowns for date selection.
 * -------------------------------------------------------------
 */
include_once dirname(__FILE__) . '/.make_timestamp.php';
include_once dirname(__FILE__) . '/function.html_options.php';
function smarty_function_html_select_date($params, &$smarty)
{
    /* Default values. */
    $prefix          = "Date_";
    $time            = time();
    $start_year      = strftime("%Y");
    $end_year        = $start_year;
    $display_days    = true;
    $display_months  = true;
    $display_years   = true;
    $month_format    = "%B";
    $day_format      = "%02d";
    $year_as_text    = false;
    /* Display years in reverse order? Ie. 2000,1999,.... */
    $reverse_years   = false;
    /* Should the select boxes be part of an array when returned from PHP?
       e.g. setting it to "birthday", would create "birthday[Day]",
       "birthday[Month]" & "birthday[Year]". Can be combined with prefix */
    $field_array     = null;
    /* <select size>'s of the different <select> tags.
       If not set, uses default dropdown. */
    $day_size        = null;
    $month_size      = null;
    $year_size       = null;
    /* Unparsed attributes common to *ALL* the <select>/<input> tags.
       An example might be in the template: all_extra ='class ="foo"'. */
    $all_extra       = null;
    /* Separate attributes for the tags. */
    $day_extra       = null;
    $month_extra     = null;
    $year_extra      = null;
    /* Order in which to display the fields.
       "D" -> day, "M" -> month, "Y" -> year. */
    $field_order      = 'MDY';
    /* String printed between the different fields. */
    $field_separator = "\n";

    extract($params);

    $time = smarty_make_timestamp($time);
    $field_order = strtoupper($field_order);

    $html_result = $month_result = $day_result = $year_result = "";

    if ($display_months) {
        $month_names = array();

        for ($i = 1; $i <= 12; $i++)
            $month_names[] = strftime($month_format, mktime(0, 0, 0, $i, 1, 2000));

        $month_result .= '<select name=';
        if (null !== $field_array){
            $month_result .= '"' . $field_array . '[' . $prefix . 'Month]"';
        } else {
            $month_result .= '"' . $prefix . 'Month"';
        }
        if (null !== $month_size){
            $month_result .= ' size="' . $month_size . '"';
        }
        if (null !== $month_extra){
            $month_result .= ' ' . $month_extra;
        }
        if (null !== $all_extra){
            $month_result .= ' ' . $all_extra;
        }
        $month_result .= '>'."\n";
        $month_result .= smarty_function_html_options(array('output'     => $month_names,
                                                            'values'     => range(1, 12),
                                                            'selected'   => strftime("%m", $time),
                                                            'print_result' => false),
                                                      $smarty);
        $month_result .= '</select>';
    }

    if ($display_days) {
        $days = range(1, 31);
        for ($i = 0; $i < count($days); $i++)
            $days[$i] = sprintf($day_format, $days[$i]);

        $day_result .= '<select name=';
        if (null !== $field_array){
            $day_result .= '"' . $field_array . '[' . $prefix . 'Day]"';
        } else {
            $day_result .= '"' . $prefix . 'Day"';
        }
        if (null !== $day_size){
            $day_result .= ' size="' . $day_size . '"';
        }
        if (null !== $all_extra){
            $day_result .= ' ' . $all_extra;
        }
        if (null !== $day_extra){
            $day_result .= ' ' . $day_extra;
        }
        $day_result .= '>'."\n";
        $day_result .= smarty_function_html_options(array('output'     => $days,
                                                          'values'     => range(1, 31),
                                                          'selected'   => strftime("%d", $time),
                                                          'print_result' => false),
                                                    $smarty);
        $day_result .= '</select>';
    }

    if ($display_years) {
        if (null !== $field_array){
            $year_name = $field_array . '[' . $prefix . 'Year]';
        } else {
            $year_name = $prefix . 'Year';
        }
        if ($year_as_text) {
            $year_result .= '<input type="text" name="' . $year_name . '" value="'.strftime('%Y', $time).'" size="4" maxlength="4"';
            if (null !== $all_extra){
                $year_result .= ' ' . $all_extra;
            }
            if (null !== $year_extra){
                $year_result .= ' ' . $year_extra;
            }
            $year_result .= '>';
        } else {
            $years = range((int)$start_year, (int)$end_year);
            if ($reverse_years) {
                rsort($years, SORT_NUMERIC);
            }

            $year_result .= '<select name="' . $year_name . '"';
            if (null !== $year_size){
                $year_result .= ' size="' . $year_size . '"';
            }
            if (null !== $all_extra){
                $year_result .= ' ' . $all_extra;
            }
            if (null !== $year_extra){
                $year_result .= ' ' . $year_extra;
            }
            $year_result .= '>'."\n";
            $year_result .= smarty_function_html_options(array('output' => $years,
                                                               'values' => $years,
                                                               'selected'   => strftime("%Y", $time),
                                                               'print_result' => false),
                                                         $smarty);
            $year_result .= '</select>';
        }
    }

    // Loop thru the field_order field
    for ($i = 0; $i <= 2; $i++){
      $c = substr($field_order, $i, 1);
      switch ($c){
        case 'D':
            $html_result .= $day_result;
            break;

        case 'M':
            $html_result .= $month_result;
            break;

        case 'Y':
            $html_result .= $year_result;
            break;
      }
      // Add the field seperator
      $html_result .= $field_separator;
    }

    print $html_result;
}

/* vim: set expandtab: */

?>

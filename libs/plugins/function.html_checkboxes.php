<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * File:       function.html_checkboxes.php
 * Type:       function
 * Name:       html_checkboxes
 * Version:    1.0
 * Date:       24.Feb.2003
 * Purpose:    Prints out a list of checkbox input types
 * Input:      name       (optional) - string default "checkbox"
 *             values     (required) - array
 *             options    (optional) - associative array
 *             checked    (optional) - array default not set
 *             separator  (optional) - ie <br> or &nbsp;
 *             output     (optional) - without this one the buttons don't have names
 * Author:     Christopher Kvarme <christopher.kvarme@flashjab.com>
 * Credits:    Monte Ohrt <monte@ispi.net>
 * Examples:   {html_checkboxes values=$ids output=$names}
 *             {html_checkboxes values=$ids name='box' separator='<br>' output=$names}
 *             {html_checkboxes values=$ids checked=$checked separator='<br>' output=$names}
 * -------------------------------------------------------------
 */
function smarty_function_html_checkboxes($params, &$smarty)
{
   require_once $smarty->_get_plugin_filepath('shared','escape_special_chars');

   $name = 'checkbox';
   $values = null;
   $options = null;
   $selected = null;
   $separator = '';
   $output = null;

   $extra = '';

   foreach($params as $_key => $_val) {
      switch($_key) {
      case 'name':
      case 'separator':
         $$_key = $_val;
         break;

      case 'options':
         $$_key = (array)$_val;
         break;

      case 'values':
      case 'output':
         $$_key = array_values((array)$_val);
	 break;

      case 'checked':
      case 'selected':
         $selected = array_values((array)$_val);
         break;

      case 'checkboxes':
         $smarty->trigger_error('html_checkboxes: the use of the "checkboxes" attribute is deprecated, use "options" instead', E_USER_WARNING);
         $options = (array)$_val;
         break;

      default:
         $extra .= ' '.$_key.'="'.smarty_function_escape_special_chars((string)$_val).'"';
         break;
      }
   }

   if (!isset($options) && !isset($values))
      return ''; /* raise error here? */

   settype($selected, 'array');
   $_html_result = '';

   if (is_array($options)) {

      foreach ($options as $_key=>$_val)
         $_html_result .= smarty_function_html_checkboxes_output($name, $_key, $_val, $selected, $extra, $separator);


   } else {
      foreach ($values as $_i=>$_key) {
         $_val = isset($output[$_i]) ? $output[$_i] : '';
         $_html_result .= smarty_function_html_checkboxes_output($name, $_key, $_val, $selected, $extra, $separator);
      }

   }

   return $_html_result;

}

function smarty_function_html_checkboxes_output($name, $value, $output, $selected, $extra, $separator) {
   $_output = '<input type="checkbox" name="'
      . smarty_function_escape_special_chars($name) . '[]" value="'
      . smarty_function_escape_special_chars($value) . '"';

   if (in_array($value, $selected)) {
      $_output .= ' checked="checked"';
   }
   $_output .= $extra . ' />' . $output . $separator . "\n";

   return $_output;
}

?>

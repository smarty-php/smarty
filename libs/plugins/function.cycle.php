<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage PluginsFunction
 */

/**
 * Smarty {cycle} function plugin
 *
 * Type:     function<br>
 * Name:     cycle<br>
 * Date:     May 3, 2002<br>
 * Purpose:  cycle through given values<br>
 *
 * Examples:<br>
 * <pre>
 * {cycle values="#eeeeee,#d0d0d0d"}
 * {cycle name=row values="one,two,three" reset=true}
 * {cycle name=row}
 * </pre>
 * @link http://smarty.php.net/manual/en/language.function.cycle.php {cycle}
 *       (Smarty online manual)
 * @author Monte Ohrt <monte at ohrt dot com>
 * @author credit to Mark Priatel <mpriatel@rogers.com>
 * @author credit to Gerard <gerard@interfold.com>
 * @author credit to Jason Sweat <jsweat_php@yahoo.com>
 * @param array $params parameters
 * Input:
 *         - name = name of cycle (optional)
 *         - values = comma separated list of values to cycle,
 *                    or an array of values to cycle
 *                    (this can be left out for subsequent calls)
 *         - reset = boolean - resets given var to true
 *         - print = boolean - print var or not. default is true
 *         - advance = boolean - whether or not to advance the cycle
 *         - delimiter = the value delimiter, default is ","
 *         - assign = boolean, assigns to template var instead of
 *                    printed.
 * @param object $smarty Smarty object
 * @param object $template template object
 * @return string|null
 */
function smarty_function_cycle($params, $smarty, $template)
{
    $name = (empty($params['name'])) ? 'default' : $params['name'];
    $print = (isset($params['print'])) ? (bool)$params['print'] : true;
    $advance = (isset($params['advance'])) ? (bool)$params['advance'] : true;
    $reset = (isset($params['reset'])) ? (bool)$params['reset'] : false;
            
    if (!in_array('values', array_keys($params))) {
        if(!isset($template->plugin_data['cycle'][$name]['values'])) {
            trigger_error("cycle: missing 'values' parameter",E_USER_WARNING);
            return;
        }
    } else {
        if(isset($template->plugin_data['cycle'][$name]['values'])
            && $template->plugin_data['cycle'][$name]['values'] != $params['values'] ) {
            $template->plugin_data['cycle'][$name]['index'] = 0;
        }
        $template->plugin_data['cycle'][$name]['values'] = $params['values'];
    }

    if (isset($params['delimiter'])) {
      $template->plugin_data['cycle'][$name]['delimiter'] = $params['delimiter'];
    } elseif (!isset($template->plugin_data['cycle'][$name]['delimiter'])) {
      $template->plugin_data['cycle'][$name]['delimiter'] = ',';
    }
    
    if(is_array($template->plugin_data['cycle'][$name]['values'])) {
        $cycle_array = $template->plugin_data['cycle'][$name]['values'];
    } else {
        $cycle_array = explode($template->plugin_data['cycle'][$name]['delimiter'],$template->plugin_data['cycle'][$name]['values']);
    }
    
    if(!isset($template->plugin_data['cycle'][$name]['index']) || $reset ) {
        $template->plugin_data['cycle'][$name]['index'] = 0;
    }
    
    if (isset($params['assign'])) {
        $print = false;
        $template->assign($params['assign'], $cycle_array[$template->plugin_data['cycle'][$name]['index']]);
    }
        
    if($print) {
        $retval = $cycle_array[$template->plugin_data['cycle'][$name]['index']];
    } else {
        $retval = null;
    }

    if($advance) {
        if ( $template->plugin_data['cycle'][$name]['index'] >= count($cycle_array) -1 ) {
            $template->plugin_data['cycle'][$name]['index'] = 0;
        } else {
            $template->plugin_data['cycle'][$name]['index']++;
        }
    }
    
    return $retval;
}
?>

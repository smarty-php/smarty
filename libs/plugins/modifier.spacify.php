<?php
/**
* Smarty plugin
* 
* @package Smarty
* @subpackage PluginsModifier
*/

/**
* Smarty spacify modifier plugin
* 
* Type:     modifier<br>
* Name:     spacify<br>
* Purpose:  add spaces between characters in a string
* 
* @link http://smarty.php.net/manual/en/language.modifier.spacify.php spacify (Smarty online manual)
* @author Monte Ohrt <monte at ohrt dot com> 
* @param string $ 
* @param string $ 
* @return string 
*/
function smarty_modifier_spacify($string, $spacify_char = ' ')
{
    $smarty = Smarty::instance();
    if ($smarty->has_mb) {
        return implode($spacify_char, mb_split('//', $string, -1));
    } else {
        return implode($spacify_char, split('//', $string, -1));
    } 
} 

?>

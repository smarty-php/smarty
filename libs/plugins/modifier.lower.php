<?php
/**
* Smarty plugin
* 
* @package Smarty
* @subpackage PluginsModifier
*/

/**
* Smarty lower modifier plugin
* 
* Type:     modifier<br>
* Name:     lower<br>
* Purpose:  convert string to lowercase
* 
* @link http://smarty.php.net/manual/en/language.modifier.lower.php lower (Smarty online manual)
* @author Monte Ohrt <monte at ohrt dot com> 
* @param string $ 
* @return string 
*/
function smarty_modifier_lower($string)
{
    $smarty = Smarty::instance();
    if ($smarty->has_mb) {
        return mb_strtolower($string);
    } else {
        return strtolower($string);
    } 
} 

?>

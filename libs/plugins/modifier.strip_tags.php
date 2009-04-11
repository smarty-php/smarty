<?php
/**
* Smarty plugin
* 
* @package Smarty
* @subpackage PluginsModifier
*/

/**
* Smarty strip_tags modifier plugin
* 
* Type:     modifier<br>
* Name:     strip_tags<br>
* Purpose:  strip html tags from text
* 
* @link http://smarty.php.net/manual/en/language.modifier.strip.tags.php strip_tags (Smarty online manual)
* @author Monte Ohrt <monte at ohrt dot com> 
* @param string $ 
* @param boolean $ 
* @return string 
*/
function smarty_modifier_strip_tags($string, $replace_with_space = true)
{
    $smarty = Smarty::instance();
    if ($replace_with_space) {
        if ($smarty->has_mb) {
            return mb_ereg_replace('!<[^>]*?>!', ' ', $string, 'p');
        } else {
            return preg_replace('!<[^>]*?>!', ' ', $string);
        } 
    } else {
        return strip_tags($string);
    } 
} 

?>

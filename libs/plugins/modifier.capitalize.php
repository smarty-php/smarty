<?php
/**
 * Smarty plugin
 * @package Smarty
 * @subpackage plugins
 */


/**
 * Smarty capitalize modifier plugin
 *
 * Type:     modifier<br>
 * Name:     capitalize<br>
 * Purpose:  capitalize words in the string
 * @link http://smarty.php.net/manual/en/language.modifiers.php#LANGUAGE.MODIFIER.CAPITALIZE
 *      capitalize (Smarty online manual)
 * @param string
 * @return string
 */
function smarty_modifier_capitalize($string)
{
    return preg_replace_callback('!\b\w+\b!', 'smarty_modifier_capitalize_ucfirst', $string);    
}

function smarty_modifier_capitalize_ucfirst($string)
{
    if(!preg_match('!\d!',$string[0]))
        return ucfirst($string[0]);
    else
        return $string[0];
}


?>

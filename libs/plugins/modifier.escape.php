<?php

/*
 * Smarty plugin
 * ------------------------------------------------------------
 * Type:     modifier
 * Name:     escape
 * Purpose:  Escape the string according to escapement type
 * ------------------------------------------------------------
 */
function smarty_modifier_escape($string, $esc_type = 'html')
{
    switch ($esc_type) {
        case 'html':
            return htmlspecialchars($string, ENT_QUOTES);

        case 'url':
            return urlencode($string);

        case 'quotes':
            // escape unescaped single quotes
            return preg_replace("%(?<!\\\\)'%", "\\'", $string);

        default:
            return $string;
    }
}

/* vim: set expandtab: */

?>

<?php
/**
* Smarty plugin to execute PHP code
* 
* @package Smarty
* @subpackage PluginsBlock
* @author Uwe Tews 
*/

/**
* Smarty {php}{/php} block plugin
* 
* @param string $content contents of the block
* @param object $smarty Smarty object
* @param boolean $ &$repeat repeat flag
* @param object $template template object
* @return string content re-formatted
*/
function smarty_block_php($params, $content, $smarty, &$repeat, $template)
{ 
    if (!$smarty->allow_php_tag) {
        throw new Exception("{php} is deprecated, set allow_php_tag = true to enable");
    } 
    eval($content);
    return '';
}
?>

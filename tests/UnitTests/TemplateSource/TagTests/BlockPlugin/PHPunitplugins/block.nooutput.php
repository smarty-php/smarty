<?php
/**
 * Smarty plugin for testing block plugins
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

/**
 * Smarty {nooutput}{/nooutput} block plugin
 *
 * @param array       $params parameter array
 * @param string $content  contents of the block
 * @param object $template template object
 * @param  bool      $repeat flag
 *
 * @return string content re-formatted
 */
function smarty_block_nooutput($params, $content, $template, &$repeat)
{
    if (isset($content)) {
           $repeat = false;
    }
}

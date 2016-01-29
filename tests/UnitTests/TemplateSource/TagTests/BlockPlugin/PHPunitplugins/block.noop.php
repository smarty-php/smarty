<?php
/**
 * Smarty plugin for testing block plugins
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

/**
 * Smarty {noop}{/noop} block plugin
 *
 * @param array       $params parameter array
 * @param string $content  contents of the block
 * @param object $template template object
 * @param  bool      $repeat flag
 *
 * @return string content re-formatted
 */
function smarty_block_noop($params, $content, $template, &$repeat)
{
    if (isset($content)) {
           return $content;
    }
}

<?php
/**
 * Smarty plugin for testing block plugins
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

/**
 * Smarty {teststack}{/teststack} block plugin
 *
 * @param array       $params parameter array
 * @param string $content  contents of the block
 * @param object $template template object
 * @param  bool      $repeat flag
 *
 * @return string content re-formatted
 */
function smarty_block_teststack($params, $content, $template, &$repeat)
{
    if (isset($content)) {
        $stack = $template->smarty->_cache['_tag_stack'];
        return $stack[0][0] . '-' . $stack[1][0];
    }
}

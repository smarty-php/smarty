<?php
/**
 * Smarty plugin for testing block plugins
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */
/**
 * Smarty {testparameter}{/testparameter} block plugin
 *
 * @param array                     $params   parameter array
 * @param string                    $content  contents of the block
 * @param \Smarty_Internal_Template $template template object
 * @param  bool                     $repeat   flag
 *
 * @return string content re-formatted
 */
function smarty_block_testparameter($params, $content, Smarty_Internal_Template $template, &$repeat)
{
    if (isset($content)) {
        return $content;
    } else {
        $template->assign('foo', $params[ 'value' ]);
    }
}

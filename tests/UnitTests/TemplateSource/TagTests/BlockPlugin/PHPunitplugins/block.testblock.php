<?php
/**
 * Smarty plugin for testing block plugins
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */

/**
 * Smarty {testblock}{/testblock} block plugin
 *
 * @param array       $params parameter array
 * @param string $content  contents of the block
 * @param object $template template object
 * @param  bool      $repeat flag
 *
 * @return string content re-formatted
 */
function smarty_block_testblock($params, $content, $template, &$repeat)
{
    static $loop = 0;
    if (isset($content)) {
        $loop ++;
        if ($loop < 5) {
            $repeat = true;
        } else {
            $repeat = false;
        }
       return $loop;
    } else {
        $loop = 0;
    }
}

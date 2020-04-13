<?php
/**
 * Smarty block plugin dummy for testing plugin functionallity
 *
 * @package    Smarty
 * @subpackage PHPunitPlugin
 */
/**
 * Smarty {dummyblock} {/dummyblock}
 *
 * @param $params
 * @param $content
 * @param $template
 * @param $repeat
 */
function smarty_block_dummyblock($params, $content, $template, &$repeat)
{
    if (is_null($content)) {
        return;
    } else {
        return $content;
    }
}

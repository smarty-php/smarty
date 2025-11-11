<?php
/**
 * Smarty Internal Plugin Compile Continue
 * Compiles the {continue} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

/**
 * Smarty Internal Plugin Compile Continue Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
// PHP 8.2+: Allow dynamic properties for compiler state and tag-specific data
#[\AllowDynamicProperties]
class Smarty_Internal_Compile_Continue extends Smarty_Internal_Compile_Break
{
    /**
     * Tag name
     *
     * @var string
     */
    public $tag = 'continue';
}

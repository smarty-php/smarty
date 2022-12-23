<?php
/**
 * Smarty Internal Plugin Compile Continue
 * Compiles the {continue} tag
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compile\Tag;

/**
 * Smarty Internal Plugin Compile Continue Class
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class ContinueTag extends BreakTag {

	/**
	 * Tag name
	 *
	 * @var string
	 */
	protected $tag = 'continue';
}

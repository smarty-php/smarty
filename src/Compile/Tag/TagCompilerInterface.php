<?php

namespace Smarty\Compile\Tag;

/**
 * This class does extend all internal compile plugins
 *
 * @package    Smarty
 * @subpackage Compiler
 */
interface TagCompilerInterface {

	/**
	 * Compiles code for the tag
	 *
	 * @param array $args array with attributes from parser
	 * @param \Smarty\Compiler\Template $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return bool|string compiled code or true if no code has been compiled
	 * @throws \Smarty\CompilerException
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null);

	public function isCacheable(): bool;
}
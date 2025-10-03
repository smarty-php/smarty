<?php

namespace Smarty\FunctionHandler;

/**
 * Function handler interface with support for specifying supported properties
 */
interface AttributeFunctionHandlerInterface extends FunctionHandlerInterface
{
	/**
	 * Returns an array with the supported attributes, flags, and shorttags
	 * @return array<string, array>
	 */
	public function getSupportedAttributes(): array;
}

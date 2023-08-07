<?php

namespace Smarty\FunctionHandler;

use Smarty\Exception;
use Smarty\Template;

/**
 * Get string length
 *
 *  strlen(string $string): int
 *
 * Returns length of the string on success, and 0 if the string is empty.
 */
class Strlen extends Base {

	public function handle($params, Template $template) {

		$params = array_values($params ?? []);

		if (count($params) !== 1) {
			throw new Exception("Invalid number of arguments for strlen. strlen expects exactly 1 parameter.");
		}

		return strlen((string) $params[0]);
	}

}
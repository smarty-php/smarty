<?php

namespace Smarty\FunctionHandler;

use Smarty\Exception;
use Smarty\Template;

/**
 *  in_array(mixed $needle, array $haystack, bool $strict = false): bool
 *  Returns true if needle is found in the array, false otherwise
 */
class InArray extends Base {

	public function handle($params, Template $template) {

		$params = array_values($params ?? []);

		if (count($params) < 2 || count($params) > 3) {
			throw new Exception("Invalid number of arguments for in_array. in_arrays expects 2 or 3 parameters.");
		}

		// default to false, true if param 3 is set to true
		$needle = $params[0];
		$haystack = (array) $params[1];
		$strict = count($params) == 3 && $params[2];

		return in_array($needle, $haystack, $strict);
	}

}
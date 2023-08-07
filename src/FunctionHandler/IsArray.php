<?php

namespace Smarty\FunctionHandler;

use Smarty\Exception;
use Smarty\Template;

/**
 *  is_array(mixed $value): bool
 *  Returns true if value is an array, false otherwise.
 */
class IsArray extends Base {

	public function handle($params, Template $template) {
		if (count($params) !== 1) {
			throw new Exception("Invalid number of arguments for is_array. is_array expects exactly 1 parameter.");
		}
		return is_array(reset($params));
	}

}
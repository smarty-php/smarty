<?php

namespace Smarty\FunctionHandler;

use Smarty\Exception;
use Smarty\Template;

/**
 * empty(mixed $var): bool
 *
 * Returns true if var does not exist or has a value that is empty or equal to zero, aka falsey, see conversion to
 * boolean. Otherwise returns false.
 *
 * No warning is generated if the variable does not exist. That means empty() is essentially the concise equivalent
 * to !isset($var) || $var == false.
 */
class EmptyHandler extends Base {

	public function handle($params, Template $template) {

		if (count($params) !== 1) {
			throw new Exception("Invalid number of arguments for empty. empty expects exactly 1 parameter.");
		}
		return empty(reset($params));
	}

}
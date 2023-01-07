<?php

namespace Smarty\FunctionHandler;

use Smarty\Exception;
use Smarty\Template;

/**
 *  is_array(mixed $value): bool
 *  Returns true if value is an array, false otherwise.
 */
class Time extends Base {

	public function handle($params, Template $template) {
		if (count($params) > 0) {
			throw new Exception("Invalid number of arguments for time. time expects no parameters.");
		}
		return time();
	}

}
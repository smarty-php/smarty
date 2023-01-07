<?php

namespace Smarty\FunctionHandler;

use Smarty\Exception;
use Smarty\Template;

/**
 * Determines if a variable is declared and is different than null
 * `isset(mixed $var, mixed ...$vars): bool`
 *
 * Returns true if var exists and has any value other than null. false otherwise.
 */
class IssetHandler extends Base {

	public function handle($params, Template $template) {

		if (count($params) === 0) {
			throw new Exception("Invalid number of arguments for isset. isset expects at least 1 parameter.");
		}

		foreach ($params as $param) {
			if (!isset($param)) {
				return false;
			}
		}
		return true;
	}

}
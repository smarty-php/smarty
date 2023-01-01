<?php

namespace Smarty\Compiler;

use Smarty\Smarty;

abstract class BaseCompiler {

	/**
	 * Smarty object
	 *
	 * @var Smarty
	 */
	public $smarty = null;

	/**
	 * @return Smarty|null
	 */
	public function getSmarty(): Smarty {
		return $this->smarty;
	}

}
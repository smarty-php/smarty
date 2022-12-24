<?php

namespace Smarty\Compile\Tag;

class BCPluginWrapper extends Base {

	private $callback;

	public function __construct($callback) {
		$this->callback = $callback;
	}

	/**
	 * @inheritDoc
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		return call_user_func($this->callback, $args, $compiler->smarty);
	}
}
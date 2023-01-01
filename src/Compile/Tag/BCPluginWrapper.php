<?php

namespace Smarty\Compile\Tag;

class BCPluginWrapper extends Base {

	private $callback;

	public function __construct($callback, bool $cacheable = true) {
		$this->callback = $callback;
		$this->cacheable = $cacheable;
	}

	/**
	 * @inheritDoc
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {
		return call_user_func($this->callback, $args, $compiler->smarty);
	}
}
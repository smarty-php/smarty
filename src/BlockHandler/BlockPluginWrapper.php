<?php

namespace Smarty\BlockHandler;

use Smarty\Template;

class BlockPluginWrapper implements BlockHandlerInterface {

	private $callback;

	public function __construct($callback) {
		$this->callback = $callback;
	}

	public function handle($params, $content, Template $template, &$repeat) {
		return call_user_func_array($this->callback, [$params, $content, $template, &$repeat]);
	}
}
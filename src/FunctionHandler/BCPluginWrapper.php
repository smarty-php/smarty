<?php

namespace Smarty\FunctionHandler;

use Smarty\Template;

class BCPluginWrapper extends Base {

	private $callback;

	public function __construct($callback, bool $cacheable = true, array $cache_attributes = []) {
		$this->callback = $callback;
		$this->cacheable = $cacheable;
		$this->cache_attributes = $cache_attributes;
	}

	public function handle($params, Template $template) {
		$func = $this->callback;
		return $func($params, $template->smarty);
	}

}
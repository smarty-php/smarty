<?php

namespace Smarty\FunctionHandler;

use Smarty\Template;

class Base implements FunctionHandlerInterface {

	/**
	 * @var bool
	 */
	protected $cacheable = true;

	/**
	 * @var array
	 */
	protected $cache_attributes = [];

	public function isCacheable(): bool {
		return $this->cacheable;
	}

	public function getCacheAttributes(): array {
		return $this->cache_attributes;
	}

	public function handle($params, Template $template) {
		// TODO: Implement handle() method.
	}
}
<?php

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

class BCPluginWrapper extends Base {

	/**
	 * Attribute definition: Overwrites base class.
	 *
	 * @var array
	 * @see Smarty_Internal_CompileBase
	 */
	public $optional_attributes = array('_any');

	private $callback;

	public function __construct($callback, bool $cacheable = true) {
		$this->callback = $callback;
		$this->cacheable = $cacheable;
	}

        /**
         * Returns attribute index for unnamed ("shorthand") attribute, or null if not allowed.
         *
         * For compiler plugins, we allow arbitrarily many unnamed attributes,
         * and just make them accessible in the order they are set.
         *
	 * @param string|int|null $key Index of the argument. Type should probably be narrowed to int
         *
         * @return string|int|null
         */
        protected function getShorthandOrder($key) {
            return $key;
        }

	/**
	 * @inheritDoc
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null): string
	{
		return call_user_func($this->callback, $this->getAttributes($compiler, $args), $compiler->getSmarty());
	}
}

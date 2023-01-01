<?php

// compiler.testclose.php
use Smarty\Compile\Tag\Base;

class smarty_compiler_testclose extends Base
{

	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null) {

		$this->closeTag($compiler, 'test');

		return '';
	}
}
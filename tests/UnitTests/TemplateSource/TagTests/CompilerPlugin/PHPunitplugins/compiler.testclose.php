<?php

// compiler.testclose.php
use Smarty\Compile\Base;

class smarty_compiler_testclose extends Base
{

	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null): string
	{

		$this->closeTag($compiler, 'test');

		return '';
	}
}
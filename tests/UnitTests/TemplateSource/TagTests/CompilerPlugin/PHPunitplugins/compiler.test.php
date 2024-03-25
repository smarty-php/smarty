<?php

// compiler.test.php
use Smarty\Compile\Base;

class smarty_compiler_test extends Base
{

	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null): string
	{
        $this->required_attributes = array('data');

        $_attr = $this->getAttributes($compiler, $args);

        $this->openTag($compiler, 'test');

        return "<?php echo 'test output'; ?>";
    }
}

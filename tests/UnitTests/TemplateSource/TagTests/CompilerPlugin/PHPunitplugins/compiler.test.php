<?php

// compiler.test.php
use Smarty\Compile\Base;

class smarty_compiler_test extends Base
{
    public function compile($args, $compiler)
    {
        $this->required_attributes = array('data');

        $_attr = $this->getAttributes($compiler, $args);

        $this->openTag($compiler, 'test');

        return "<?php echo 'test output'; ?>";
    }
}

// compiler.testclose.php
class smarty_compiler_testclose extends Base
{
    public function compile($args, $compiler)
    {

        $this->closeTag($compiler, 'test');

        return '';
    }
}

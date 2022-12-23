<?php

// compiler.testclose.php
use Smarty\Compile\Tag\Base;

class smarty_compiler_testclose extends Base
{
    public function execute($args, $compiler)
    {

        $this->closeTag($compiler, 'test');

        return '';
    }
}

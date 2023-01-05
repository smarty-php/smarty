<?php
/**
 * Smarty PHPunit tests compiler errors
 *

 * @author  Uwe Tews
 */

/**
 * class for compiler tests
 *
 * 
 * @preserveGlobalState    disabled
 *
 */
class CompileErrorTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test none existing template file error
     */
    public function testNoneExistingTemplateError()
    {
        $this->expectException(\Smarty\Exception::class);
        $this->expectExceptionMessage('no.tpl');
        $this->smarty->fetch('eval:{include file=\'no.tpl\'}');
    }

    /**
     * test unkown tag error
     */
    public function testUnknownTagError()
    {
        $this->expectException(\Smarty\CompilerException::class);
        $this->expectExceptionMessage('unknown tag \'unknown\'');
        $this->smarty->fetch('eval:{unknown}');
    }

    /**
     * test unclosed tag error
     */
    public function testUnclosedTagError()
    {
        $this->expectException(\Smarty\CompilerException::class);
        $this->expectExceptionMessage('unclosed {if} tag');
        $this->smarty->fetch('eval:{if true}');
    }

    /**
     * test syntax error
     */
    public function testSyntaxError()
    {
        $this->expectException(\Smarty\CompilerException::class);
        $this->expectExceptionMessage('Unexpected "}"');
        $this->smarty->fetch('eval:{assign var=}');
    }
}

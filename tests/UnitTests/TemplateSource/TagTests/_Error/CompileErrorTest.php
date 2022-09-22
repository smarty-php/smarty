<?php
/**
 * Smarty PHPunit tests compiler errors
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for compiler tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileErrorTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
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
        $this->expectException('SmartyException');
        $this->expectExceptionMessage('no.tpl');
        $this->smarty->fetch('eval:{include file=\'no.tpl\'}');
    }

    /**
     * test unkown tag error
     */
    public function testUnknownTagError()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('unknown tag \'unknown\'');
        $this->smarty->fetch('eval:{unknown}');
    }

    /**
     * test unclosed tag error
     */
    public function testUnclosedTagError()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('unclosed {if} tag');
        $this->smarty->fetch('eval:{if true}');
    }

    /**
     * test syntax error
     */
    public function testSyntaxError()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('Unexpected "}"');
        $this->smarty->fetch('eval:{assign var=}');
    }
}

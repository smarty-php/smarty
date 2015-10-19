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
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileErrorTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * @expectedException        SmartyException
     * @expectedExceptionMessage Unable to load template file 'no.tpl' in 'eval:{include file='no.tpl'}'
     * test none existing template file error
     */
    public function testNoneExistingTemplateError()
    {
        $this->smarty->fetch('eval:{include file=\'no.tpl\'}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage unknown tag "unknown"
     * test unkown tag error
     */
    public function testUnknownTagError()
    {
        $this->smarty->fetch('eval:{unknown}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage unclosed {if} tag
     * test unclosed tag error
     */
    public function testUnclosedTagError()
    {
        $this->smarty->fetch('eval:{if true}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Unexpected "}"
     * test syntax error
     */
    public function testSyntaxError()
    {
        $this->smarty->fetch('eval:{assign var=}');
    }
}

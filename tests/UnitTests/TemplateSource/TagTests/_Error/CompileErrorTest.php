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
     * test none existing template file error
     */
    public function testNoneExistingTemplateError()
    {
        try {
            $this->smarty->fetch('eval:{include file=\'no.tpl\'}');
        }
        catch (Exception $e) {
            $this->assertContains('Unable to load template', $e->getMessage());

            return;
        }
        $this->fail('Exception for none existing template has not been raised.');
    }

    /**
     * test unkown tag error
     */
    public function testUnknownTagError()
    {
        try {
            $this->smarty->fetch('eval:{unknown}');
        }
        catch (Exception $e) {
            $this->assertContains('unknown tag "unknown"', $e->getMessage());

            return;
        }
        $this->fail('Exception for unknown Smarty tag has not been raised.');
    }

    /**
     * test unclosed tag error
     */
    public function testUnclosedTagError()
    {
        try {
            $this->smarty->fetch('eval:{if true}');
        }
        catch (Exception $e) {
            $this->assertContains('unclosed {if} tag', $e->getMessage());

            return;
        }
        $this->fail('Exception for unclosed Smarty tags has not been raised.');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax Error in template "599a9cf0e3623a3206bd02a0f5c151d5f5f3f69e"
     * @expectedExceptionMessage Unexpected "}"
     * test syntax error
     */
    public function testSyntaxError()
    {
        $this->smarty->fetch('eval:{assign var=}');
    }
}

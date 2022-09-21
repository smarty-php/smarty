<?php
/**
 * Smarty PHPunit tests for tag attributes
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for tag attribute tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AttributeTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test required attribute
     */
    public function testRequiredAttributeVar()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('missing \'var\' attribute');
        $this->smarty->fetch('string:{assign value=1}');
    }

    /**
     * test unexpected attribute
     */
    public function testUnexpectedAttribute()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('unexpected \'bar\' attribute');
        $this->smarty->fetch('string:{assign var=foo value=1 bar=2}');
    }

    /**
     * test illegal option value
     */
    public function testIllegalOptionValue()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('for option flag \'nocache\'');
        $this->expectExceptionMessage('illegal value');
        $this->smarty->fetch('string:{assign var=foo value=1 nocache=buh}');
    }

    /**
     * test too many shorthands
     */
    public function testTooManyShorthands()
    {
        $this->expectException('SmartyCompilerException');
        $this->expectExceptionMessage('too many shorthand attributes');
        $this->smarty->fetch('string:{assign foo 1 2}');
    }

    /**
      */
    public function testNumericParams()
    {
        $this->assertEquals('array(\'a\'=>\'pa\',0=>\'isnull\',)', $this->strip($this->smarty->fetch('string:{getparams a=\'pa\' 0=isnull}')));
    }


}
